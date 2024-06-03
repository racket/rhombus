#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     shrubbery/property
                     "srcloc.rkt"
                     "tag.rkt")
         syntax/parse/pre
         "provide.rkt"
         "parse.rkt"
         "expression.rkt"
         "binding.rkt"
         "pack.rkt"
         "empty-group.rkt"
         (submod "syntax-class-primitive.rkt" for-quasiquote)
         "repetition.rkt"
         "parens.rkt"
         "op-literal.rkt"
         "static-info.rkt"
         (only-in "implicit.rkt" #%parens) ; for implicit `parens` in `:esc`
         "parens-sc.rkt"
         (submod "syntax-object.rkt" for-quasiquote)
         "pattern-variable.rkt"
         "unquote-binding.rkt"
         "unquote-binding-identifier.rkt"
         "tag.rkt" ; for use in `~parse`
         "sequence-pattern.rkt")

(provide (for-spaces (#f
                      rhombus/bind
                      rhombus/repet
                      rhombus/unquote_bind)
                     #%quotes)
         (for-spaces (#f
                      rhombus/bind)
                     syntax_term)
         (for-space rhombus/unquote_bind
                    _))

(module+ convert
  (begin-for-syntax
    (provide convert-pattern
             convert-template)))

(module+ shape-dispatch
  (provide (for-syntax quoted-shape-dispatch)))

(module+ for-match
  (provide (for-syntax handle-syntax-parse-dispatch)))

(begin-for-syntax
  (define-syntax-class (list-repetition in-space)
    #:attributes (name)
    #:datum-literals (group)
    (pattern (~var || (:... in-space)))
    (pattern (group (~var || (:... in-space))))
    (pattern (_::block (group (~var || (:... in-space))))))
  (define-splicing-syntax-class (:esc dotted? any-id?)
    #:attributes (term)
    #:datum-literals (op |.|)
    (pattern (~seq a0:identifier (~seq (~and dot-op (op |.|)) a:identifier) ...+)
             #:when dotted?
             #:with term #'(parens (group a0 (~@ dot-op a) ...)))
    (pattern (~seq term)
             #:when (or any-id?
                        (not (identifier? #'term))
                        (not (unquote-binding-id? #'term))
                        (free-identifier=? (in-unquote-binding-space #'term)
                                           (unquote-bind-quote _)))))
  (define-splicing-syntax-class (:tail-repetition in-space dotted?)
    #:attributes (name term)
    (pattern (~seq (~var _ (:$ in-space)) (~var || (:esc dotted? #f)) (~var || (:... in-space)))))
  (define-splicing-syntax-class (:block-tail-repetition in-space dotted?)
    #:attributes (name term)
    #:datum-literals (group)
    (pattern (~seq (group (~var _ (:$ in-space)) (~var || (:esc dotted? #f)))
                   (group (~var || (:... in-space)))))))

(define-for-syntax (convert-syntax e make-datum make-literal make-void
                                   handle-escape handle-group-escape handle-multi-escape
                                   adjust-escape-siblings deepen-escapes deepen-syntax-escape
                                   handle-tail-escape handle-block-tail-escape
                                   handle-maybe-empty-sole-group
                                   handle-maybe-empty-alts handle-maybe-misformed-group
                                   #:in-space in-space
                                   #:tail-any-escape? [tail-any-escape? #f]
                                   #:as-tail? [as-tail? #f]
                                   #:splice? [splice? #f]
                                   #:splice-pattern [splice-pattern #f]
                                   #:allow-fltten? [allow-flatten? #f]
                                   #:make-describe-op [make-describe-op (lambda (e name) e)]
                                   #:improve-repetition-constraints [improve-repetition-constraints (lambda (ps gs) ps)])
  (let convert ([e e] [empty-ok? splice?] [depth 0] [as-tail? as-tail?] [splice? splice?])
    (syntax-parse e
      #:datum-literals (group parens brackets braces block quotes multi alts)
      [(group
        (~var $-id (:$ in-space)) (~var esc (:esc tail-any-escape? #f)))
       #:when (and (zero? depth) (not as-tail?) (not splice?))
       ;; Special case: a group whose content is an escape; the escape
       ;; defaults to "group" mode instead of "term" mode
       #:do [(define-values (p new-idrs new-sidrs new-vars) (handle-group-escape #'$-id.name #'esc.term e))]
       #:when p
       (values p new-idrs new-sidrs new-vars #f)]
      [((~or* parens brackets braces quotes multi block)
        (group (~var $-id (:$ in-space)) (~var esc (:esc tail-any-escape? #f))))
       #:when (and (zero? depth) (not as-tail?))
       ;; Analogous special case, but for blocks (maybe within an `alts`), etc.
       #:do [(define-values (p new-idrs new-sidrs new-vars) (handle-multi-escape #'$-id.name #'esc.term e splice?))]
       #:when p
       (values p new-idrs new-sidrs new-vars #f)]
      [((~and tag (~or* parens brackets braces quotes multi block))
        (~and g (group . _)))
       #:when (not splice?)
       ;; Special case: for a single group with (), [], {}, '', or block, if the group
       ;; can be empty, allow a match/construction with zero groups
       (define-values (p new-idrs new-sidrs new-vars can-be-empty?) (convert #'g #t depth as-tail? #f))
       (if can-be-empty?
           (handle-maybe-empty-sole-group #'tag p new-idrs new-sidrs new-vars)
           (values (no-srcloc #`(#,(make-datum #'tag) #,p))
                   new-idrs
                   new-sidrs
                   new-vars
                   #f))]
      [((~and tag (~or* parens brackets braces quotes multi block alts group))
        g ...)
       (define in-block? (eq? (syntax-e #'tag) 'block))
       ;; Note: this is where `depth` would be incremented, when `tag` is `quotes`, if we wanted that
       (let loop ([gs #'(g ...)] [pend-idrs #f] [pend-sidrs #f] [pend-vars #f]
                                 [idrs '()]  ; list of #`[#,id #,rhs] for definitions
                                 [sidrs '()] ; list of #`[(#,id ...) #,rhs] for syntax definitions
                                 [vars '()]  ; list of `[,id . ,depth] for visible subset of `idrs` and `sidrs`
                                 [ps '()] [can-be-empty? #t] [pend-is-splice? #f] [tail #f] [depth depth]
                                 [needs-group-check? #f])
         (define really-can-be-empty? (and can-be-empty? (or pend-is-splice? (not pend-idrs))))
         (define (simple gs a-depth)
           (syntax-parse gs
             [(g . gs)
              (define-values (p new-ids new-sidrs new-vars nested-can-be-empty?) (convert #'g #f a-depth #f #f))
              (loop #'gs new-ids new-sidrs new-vars
                    (append (or pend-idrs '()) idrs)
                    (append (or pend-sidrs '()) sidrs)
                    (append (or pend-vars '()) vars)
                    (cons p ps) really-can-be-empty? #f #f depth
                    needs-group-check?)]))
         (define (simple2 gs a-depth)
           (syntax-parse gs
             [(g0 g1 . gs)
              (define-values (p0 new-ids0 new-sids0 new-vars0 nested-can-be-empty?0) (convert #'g0 #f a-depth #f #f))
              (define-values (p1 new-ids1 new-sids1 new-vars1 nested-can-be-empty?1) (convert #'g1 #f a-depth #f #f))
              (loop #'gs (append new-ids0 new-ids1) (append new-sids0 new-sids1) (append new-vars0 new-vars1)
                    (append (or pend-idrs '()) idrs)
                    (append (or pend-sidrs '()) sidrs)
                    (append (or pend-vars '()) vars)
                    (list* p1 p0 ps) really-can-be-empty? #f #f depth
                    needs-group-check?)]))
         (define (finish ps tail idrs sidrs vars can-be-empty? needs-group-check?)
           (define (ps+tail) (if tail (append ps tail) ps))
           (cond
             [(and can-be-empty? (eq? (syntax-e #'tag) 'alts))
              (handle-maybe-empty-alts #'tag (ps+tail) idrs sidrs vars)]
             [(and (eq? (syntax-e #'tag) 'group) (or (and needs-group-check? allow-flatten? (not splice?))
                                                     (and can-be-empty? (not empty-ok?))))
              (handle-maybe-misformed-group #'tag ps tail idrs sidrs vars can-be-empty? empty-ok?)]
             [else
              (let ([ps (ps+tail)])
                (values
                 (if splice?
                     (if splice-pattern
                         (splice-pattern ps)
                         (quasisyntax/loc e (~seq . #,ps)))
                     (no-srcloc #`(#,(make-datum #'tag) . #,ps)))
                 idrs
                 sidrs
                 vars
                 can-be-empty?))]))
         (syntax-parse gs
           #:datum-literals (op)
           [()
            (finish (reverse ps) tail
                    (append (or pend-idrs '()) idrs)
                    (append (or pend-sidrs '()) sidrs)
                    (append (or pend-vars '()) vars)
                    really-can-be-empty?
                    needs-group-check?)]
           [((~var op (:tail-repetition in-space tail-any-escape?)))
            #:when (and (zero? depth)
                        (or tail-any-escape?
                            (identifier? #'op.term))
                        (or (not splice?)
                            as-tail?))
            (define-values (id new-idrs new-sidrs new-vars) (handle-tail-escape #'op.name #'op.term e))
            (loop #'() #f #f #f
                  (append new-idrs (or pend-idrs '()) idrs)
                  (append new-sidrs (or pend-sidrs '()) sidrs)
                  (append new-vars (or pend-vars '()) vars)
                  ps really-can-be-empty? #f id depth
                  ;; tail escape is responsible for making sure it's valid
                  needs-group-check?)]
           [((~var op (:block-tail-repetition in-space tail-any-escape?)))
            #:when (and (zero? depth)
                        (or tail-any-escape?
                            (identifier? #'op.term)))
            (define-values (id new-idrs new-sidrs new-vars) (handle-block-tail-escape #'op.name #'op.term e))
            (loop #'() #f #f #f
                  (append new-idrs (or pend-idrs '()) idrs)
                  (append new-sidrs (or pend-sidrs '()) sidrs)
                  (append new-vars (or pend-vars '()) vars)
                  ps really-can-be-empty? #f id depth
                  #f)]
           [((~var op (list-repetition in-space)) . gs)
            #:when (zero? depth)
            (unless pend-idrs
              (raise-syntax-error #f
                                  "misplaced repetition"
                                  #'op.name))
            (define adj-pend-idrs (adjust-escape-siblings pend-idrs))
            (define new-pend-idrs (deepen-escapes adj-pend-idrs))
            (define new-pend-sidrs (for/list ([sidr (in-list pend-sidrs)])
                                     (deepen-syntax-escape sidr)))
            (define new-pend-vars (for/list ([var (in-list pend-vars)])
                                    (struct-copy pattern-variable var
                                                 [depth (+ 1 (pattern-variable-depth var))])))
            (if allow-flatten?
                (loop #'gs new-pend-idrs new-pend-sidrs new-pend-vars
                      idrs
                      sidrs
                      vars
                      (cons (quote-syntax ...) ps) can-be-empty? #t #f depth
                      #t)
                (let ([ps (if (eq? (syntax-e #'tag) 'group)
                              (improve-repetition-constraints ps #'gs)
                              ps)])
                  (loop #'gs #f #f #f
                        (append new-pend-idrs idrs)
                        (append new-pend-sidrs sidrs)
                        (append new-pend-vars vars)
                        (cons (quote-syntax ...) ps) can-be-empty? #f #f depth
                        #t)))]
           [((op (~var $-id (:$ in-space))) (~var esc (:esc tail-any-escape? #t)) . n-gs)
            (cond
              [(zero? depth)
               (define tail? (and (null? (syntax-e #'n-gs))
                                  (or (and (not tail) (not splice?)) as-tail?)))
               (define-values (pat new-idrs new-sidrs new-vars) (handle-escape #'$-id.name #'esc.term e tail?))
               (cond
                 [tail?
                  (finish (reverse ps) pat
                          (append new-idrs (or pend-idrs '()) idrs)
                          (append new-sidrs (or pend-sidrs '()) sidrs)
                          (append new-vars (or pend-vars '()) vars)
                          really-can-be-empty?
                          ;; tail escape is responsible for ensuring its own validity
                          needs-group-check?)]
                 [else
                  (loop #'n-gs new-idrs new-sidrs new-vars
                        (append (or pend-idrs '()) idrs)
                        (append (or pend-sidrs '()) sidrs)
                        (append (or pend-vars '()) vars)
                        (cons pat ps)
                        really-can-be-empty? #t #f depth
                        #t)])]
              [else
               (simple2 gs (sub1 depth))])]
           [((op (~var $-id (:$ in-space))))
            (raise-syntax-error #f
                                "misplaced escape"
                                #'$-id.name)]
           [(g . _)
            (simple gs depth)]))]
      [((~and tag op) op-name)
       (values (make-describe-op (no-srcloc #`(#,(make-datum #'tag) #,(make-literal #'op-name))) #'op-name)
               null null null #f)]
      [id:identifier
       (values (make-literal #'id) null null null #f)]
      [void-val
       #:when (eq? (void) (syntax-e #'void-val))
       (values (make-void e) null null null #f)]
      [_
       (values e null null null #f)])))

;; Conversion produces a `syntax-parse` pattern, a list of identifiers
;; and right-hand sides to be `define`d (= "idrs"), and a list of
;; identifiers and right-hand sides to be `define-syntax`ed (=
;; "sidrs"). The idr and sidr right-hand sides have particular shape
;; that allow their nesting to be deeped as `...`s are discovered
;; around them. The sidrs are expected to refer to the idrs (but not
;; other sidrs), so idrs are bound in the "commit" step of binding,
;; while sidrs are bound at the final step, and that split cooperates
;; correctly with `let`.
(define-for-syntax (convert-pattern e
                                    #:as-tail? [as-tail? #f]
                                    #:splice? [splice? #f]
                                    #:splice-pattern [splice-pattern #f])
  (define (make-datum d)
    (case (syntax-e d)
      [(parens) #'(~var _ :parens)]
      [(block) #'(~var _ :block)]
      [(alts) #'(~var _ :alts)]
      [(braces) #'(~var _ :braces)]
      [(brackets) #'(~var _ :brackets)]
      [(quotes) #'(~var _ :quotes)]
      [else #`(~datum #,d)]))
  (define (handle-escape $-id e in-e kind)
    (define parsed
      (parameterize ([current-unquote-binding-kind kind])
        (syntax-parse #`(#,group-tag #,e)
          [esc::unquote-binding #'esc.parsed])))
    (syntax-parse parsed
      [#f (values #f #f #f #f)]
      [id:identifier
       (identifier-as-unquote-binding #'id kind
                                      #:result values
                                      #:pattern-variable pattern-variable)]
      [(pat idrs sidrs vars)
       (values #'pat
               (syntax->list #'idrs)
               (syntax->list #'sidrs)
               (for/list ([var (in-list (syntax->list #'vars))])
                 (syntax-list->pattern-variable var)))]))
  (define (handle-escape/match-head $-id e in-e kind splice?)
    (define-values (p idrs sidrs vars) (handle-escape $-id e in-e kind))
    (if p
        (values (if splice? ;; splicing `multi` means match any head
                    p
                    (syntax-parse in-e
                      [(tag . _) #`(~and (#,(make-datum #'tag) . _) #,p)]))
                idrs
                sidrs
                vars)
        (values #f #f #f #f)))
  (convert-syntax e
                  #:in-space in-binding-space
                  #:as-tail? as-tail?
                  #:splice? splice?
                  #:splice-pattern splice-pattern
                  ;; make-datum
                  make-datum
                  ;; make-literal
                  (lambda (d)
                    #`(~datum #,d))
                  ;; make-void
                  (lambda (e)
                    #`(~datum #,(void)))
                  ;; handle-escape:
                  (lambda ($-id e in-e tail?)
                    (let-values ([(p new-idrs new-sidrs new-vars)
                                  (if tail?
                                      (handle-escape $-id e in-e 'group)
                                      (values #f #f #f #f))])
                      (if p
                          (with-syntax ([(tmp) (generate-temporaries '(tail))])
                            (values #`(~and tmp (~parse #,p (cons group-tag #'tmp)))
                                    new-idrs new-sidrs new-vars))
                          (let-values ([(p new-idrs new-sidrs new-vars) (handle-escape $-id e in-e 'term)])
                            (values (if tail? (list p) p) new-idrs new-sidrs new-vars)))))
                  ;; handle-group-escape:
                  (lambda ($-id e in-e)
                    (handle-escape/match-head $-id e in-e 'group #f))
                  ;; handle-multi-escape:
                  (lambda ($-id e in-e splice?)
                    (define kind
                      (syntax-parse in-e
                        [(head . _) (if (memq (syntax-e #'head) '(block alts))
                                        'block
                                        'multi)]
                        [_ 'multi]))
                    (handle-escape/match-head $-id e in-e kind splice?))
                  ;; adjust-escape-siblings
                  (lambda (idrs)
                    idrs)
                  ;; deepen-escapes
                  (lambda (idrs)
                    (for/list ([idr (in-list idrs)])
                      (syntax-parse idr
                        #:literals (pack-nothing*)
                        [(id (pack-nothing* _ _)) idr]
                        [(id (pack (_ stx) depth))
                         #`(id (pack (syntax (stx (... ...))) #,(add1 (syntax-e #'depth))))])))
                  ;; deepen-syntax-escape
                  (lambda (sidr)
                    (deepen-pattern-variable-bind sidr))
                  ;; handle-tail-escape:
                  (lambda (name e in-e)
                    (syntax-parse e
                      [_::_-bind
                       (values #'_ null null null)]
                      [_
                       (let ([temp0-id (car (generate-temporaries (list e)))]
                             [temp-id (car (generate-temporaries (list e)))])
                         (values temp0-id
                                 (list #`[#,temp-id (pack-tail* (syntax #,temp0-id) 0)])
                                 (list (make-pattern-variable-bind e temp-id (quote-syntax unpack-tail-list*)
                                                                   1 '()))
                                 (list (pattern-variable (syntax-e e) e temp-id 1 (quote-syntax unpack-tail-list*)))))]))
                  ;; handle-block-tail-escape:
                  (lambda (name e in-e)
                    (let ([temp0-id (car (generate-temporaries (list e)))]
                          [temp-id (car (generate-temporaries (list e)))])
                      (values temp0-id
                              (list #`[#,temp-id (pack-multi-tail* (syntax #,temp0-id) 0)])
                              (list (make-pattern-variable-bind e temp-id (quote-syntax unpack-multi-tail-list*)
                                                                1 null))
                              (list (pattern-variable (syntax-e e) e temp-id 1 (quote-syntax unpack-multi-tail-list*))))))
                  ;; handle-maybe-empty-sole-group
                  (lambda (tag pat idrs sidrs vars)
                    ;; `pat` matches a `group` form that's supposed to be under `tag`,
                    ;; but if `pat` match `(group)`, then allow an overall match to `(tag)`
                    (values #`(~or* ((~datum #,tag) #,pat)
                                    (~and ((~datum #,tag))
                                          ;; sets all pattern variables to nested empties:
                                          (_ . #,(syntax-parse pat
                                                   [(_ . tail) #'tail]))))
                            idrs
                            sidrs
                            vars
                            #f))
                  ;; handle-maybe-empty-alts
                  (lambda (tag ps idrs sidrs vars)
                    ;; if `(tag . ps)` would match `(alts)`, then let it match `(block)`
                    (values #`(~or* ((~datum #,tag) . #,ps)
                                    (~and ((~datum block))
                                          ;; sets all pattern variables to nested empties:
                                          (_ . #,ps)))
                            idrs
                            sidrs
                            vars
                            #t))
                  ;; handle-maybe-misformed-group
                  (lambda (tag ps tail idrs sidrs vars can-be-empty? empty-ok?)
                    (let ([ps (if tail (append ps tail) ps)])
                      ;; the `(tag . ps)` could match `(group)` or an otherwise misformed group,
                      ;; but that shouldn't be an input
                      (values #`((~datum #,tag) . #,ps) idrs sidrs vars #t)))
                  #:make-describe-op
                  (lambda (e name)
                    #`(~describe #:opaque
                                 #,(format "the operator `~a`"
                                           (or (syntax-raw-property name)
                                               (syntax-e name)))
                                 #,e))
                  #:improve-repetition-constraints
                  (lambda (ps gs)
                    ;; The first pattern in `ps` is followed by `...`.
                    ;; If there's more in the remaining pattern `gs`, then we
                    ;; may know that repetition matches can't be `block` or `alts`
                    ;; terms, and specifying that constraint up front can avoid
                    ;; "expected more terms" messages where more terms are not
                    ;; possible. For example, when matching, '$a ...: 1' a block
                    ;; that isn't ': 1' should not be treated as an '$a' match.
                    ;; The analysis here is approximate, but should cover useful
                    ;; cases.
                    (define (no-wrap) ps)
                    (define (wrap not-pat) (cons #`(~and #,(if (is-sequence-pattern? (car ps))
                                                               #`(~seq #,not-pat (... ...))
                                                               not-pat)
                                                         #,(car ps))
                                                 (cdr ps)))
                    (define (wrap-non-alts) (wrap #`(~not ((~datum alts) . _))))
                    (define (wrap-non-block-non-alts) (wrap #`(~not ((~or* (~datum block) (~datum alts)) . _))))
                    (let loop ([gs gs])
                      (syntax-parse gs
                        [() (no-wrap)]
                        [((_::block . _) . _) (wrap-non-block-non-alts)]
                        [((_::alts . _) . _) (wrap-non-alts)]
                        [(_::$-bind _ _::...-bind . gs) (loop #'gs)]
                        [(_::$-bind . gs) (loop #'gs)]
                        [(_ _::...-bind . gs) (loop #'gs)]
                        [_ (wrap-non-block-non-alts)])))))

(define-unquote-binding-syntax #%quotes
  (unquote-binding-prefix-operator
   null
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(form-id qs . tail)
        (define ((build pat-kind) e)
          (define kind (current-unquote-binding-kind))
          (cond
            [(and (eq? pat-kind 'term)
                  (not (eq? kind 'term)))
             #'#f]
            [(and (eq? kind 'group)
                  (eq? pat-kind 'multi)
                  (syntax-parse e [(_) #t] [_ #f]))
             ;; defer special case to term context
             #'#f]
            [else
             (define-values (use-e splice?)
               (cond
                 [(and (eq? pat-kind 'multi)
                       (memq kind '(term group)))
                  ;; empty multi-group term is a special case that we can splice
                  ;; into a term context
                  (syntax-parse e
                    [(_) (values #'(group) #t)]
                    [_ (raise-syntax-error #f
                                           (format "multi-group pattern incompatible with ~a context" kind)
                                           #'qs)])]
                 [else (values e (and (eq? pat-kind 'group)
                                      (memq kind '(multi block term))))]))
             (define-values (pattern idrs sidrs vars can-be-empty?)
               (convert-pattern use-e
                                #:splice? splice?
                                #:splice-pattern (cond
                                                   [(eq? kind 'multi)
                                                    (lambda (e)
                                                      #`(_ ((~datum group) . #,e)))]
                                                   [(eq? kind 'block)
                                                    (lambda (e)
                                                      #`((~datum block) ((~datum group) . #,e)))]
                                                   [else #f])))
             #`(#,pattern #,idrs #,sidrs #,(map pattern-variable->list vars))]))
        (define-values (r empty-tail)
          (quoted-shape-dispatch #'(form-id qs)
                                 in-binding-space
                                 (build 'term)
                                 (build 'group)
                                 (build 'multi)
                                 (lambda (e)
                                   (if (eq? (current-unquote-binding-kind) 'term)
                                       #`((~datum #,e) () () ())
                                       #'#f))))
        (values r #'tail)]))))

(define-unquote-binding-syntax _
  (unquote-binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values #`(#,(syntax/loc #'form-id _) () () ())
                #'tail)]))))

(define-for-syntax (convert-template e
                                     #:check-escape [check-escape (lambda (e) (void))]
                                     #:rhombus-expression [rhombus-expression #'rhombus-expression]
                                     #:repetition? [repetition? #f])
  (syntax-parse (and (not repetition?) e)
    #:datum-literals (group multi)
    [(group _::$-expr tail:identifier dots::...-expr)
     (convert-direct-tail-template #'tail #'dots)]
    [(multi (group _::$-expr tail:identifier dots::...-expr))
     (convert-direct-tail-template #'tail #'dots)]
    [_
     (define-values (template idrs sidrs vars can-be-empty?)
       (convert-syntax e
                       #:in-space in-expression-space
                       #:tail-any-escape? #t
                       #:allow-fltten? #t
                       ;; make-datum
                       (lambda (d) d)
                       ;; make-literal
                       (lambda (d) (if (and (identifier? d)
                                            (free-identifier=? d (quote-syntax ...)))
                                       #`(#,(quote-syntax ...) #,d)
                                       d))
                       ;; make-void
                       (lambda (e) e)
                       ;; handle-escape:
                       (lambda ($-id e in-e tail?)
                         (check-escape e)
                         (define id (car (generate-temporaries (list e))))
                         (values (if tail? id #`(#,(quote-syntax ~@) . #,id))
                                 (list #`[#,id (pending-unpack #,e unpack-term-list* (quote-syntax #,$-id))]) null null))
                       ;; handle-group-escape:
                       (lambda ($-id e in-e)
                         (check-escape e)
                         (define id (car (generate-temporaries (list e))))
                         (values #`(#,(quote-syntax ~@) . #,id)
                                 (list #`[#,id (pending-unpack #,e unpack-group-list* (quote-syntax #,$-id))]) null null))
                       ;; handle-multi-escape:
                       (lambda ($-id e in-e splice?)
                         (check-escape e)
                         (define id (car (generate-temporaries (list e))))
                         (with-syntax ([(tag . _) in-e])
                           (values (no-srcloc #`(tag . #,id))
                                   (list #`[#,id (pending-unpack #,e unpack-multi* (quote-syntax #,$-id))]) null null)))
                       ;; adjust-escape-siblings
                       (lambda (idrs)
                         ;; adapt to allow repetitions at different depths where
                         ;; shallower reptitions are copied to match deeper ones
                         (adjust-template-sibling-depths idrs))
                       ;; deepen-escape
                       (lambda (idrs)
                         (deepen-template-escapes idrs))
                       ;; deepen-syntax-escape
                       (lambda (sidr)
                         (error "should have no sidrs for template"))
                       ;; handle-tail-escape:
                       (lambda (name e in-e)
                         (define id (car (generate-temporaries (list e))))
                         (syntax-parse #`(group #,e)
                           [rep::repetition
                            (values id (list #`[#,id (unpacking 1 0 rep.parsed unpack-tail* (quote-syntax #,name))]) null null)]))
                       ;; handle-block-tail-escape:
                       (lambda (name e in-e)
                         (define id (car (generate-temporaries (list e))))
                         (syntax-parse #`(group #,e)
                           [rep::repetition
                            (values id (list #`[#,id (unpacking 1 0 rep.parsed unpack-multi-tail* (quote-syntax #,name))]) null null)]))
                       ;; handle-maybe-empty-sole-group
                       (lambda (tag template idrs sidrs vars)
                         ;; if `template` generates `(group)`, then instead of `(tag (group))`,
                         ;; produce `(tag)`
                         (define id (car (generate-temporaries '(group))))
                         (values (no-srcloc #`(#,tag #,id (... ...)))
                                 (cons #`[(#,id (... ...))
                                          (convert-empty-group 0 (#,(quote-syntax quasisyntax) #,template))]
                                       idrs)
                                 sidrs
                                 vars
                                 #f))
                       ;; handle-maybe-empty-alts
                       (lambda (tag ts idrs sidrs vars)
                         ;; if `(tag . ts)` generates `(alts)`, then produce `(block)` instead
                         (define id (car (generate-temporaries '(alts))))
                         (values id
                                 (cons #`[#,id (convert-empty-alts 0 (#,(quote-syntax quasisyntax) #,(no-srcloc #`(#,tag . #,ts))))]
                                       idrs)
                                 sidrs
                                 vars
                                 #f))
                       ;; handle-maybe-misformed-group
                       (lambda (tag ts tail idrs sidrs vars can-be-empty? empty-ok?)
                         ;; Need to check that `ts` followed by an optional tail is well formed
                         ;; (e.g., no alnternativs in the middle of the group); the `tail` is
                         ;; provided separately, because we can assume that its well-formed and
                         ;; shouldn't be checked (otherwise we may create quadratic work);
                         ;; unless `empty-ok?`, then error if the group would be empty
                         (define id (car (generate-temporaries '(group))))
                         (define check-id (if empty-ok? #'check-misformed-group #'check-empty-or-misformed-group))
                         ;; all the idrs in `idrs` contribute toward the amount of repetition that is
                         ;; possible for `ts`; so record the dependency for use in `deepen-template-escapes`
                         (define ids (map extract-idr-name idrs))
                         (values id
                                 (cons #`[#,id (dependent-unpack
                                                #,ids
                                                (#,check-id
                                                 0
                                                 ;; `check-misformed-group` or `check-empty-or-misformed-group`
                                                 ;; expects a syntax-list of three parts to check and assemble
                                                 (#,(quote-syntax quasisyntax) #,(no-srcloc #`(#,tag #,ts #,(or tail null))))))]
                                       idrs)
                                 sidrs
                                 vars
                                 can-be-empty?))))

     (define-values (depth new-idrs new-template)
       (cond
         [repetition? (deepen-for-repetition idrs template)]
         [else (values 0 idrs template)]))
     (define (wrap-bindings idrs body)
       (cond
         [(null? idrs) body]
         [else
          (wrap-bindings (cdr idrs)
                         (syntax-parse (car idrs)
                           [(id-pat e)
                            #`(with-syntax ([id-pat e])
                                #,body)]))]))
     (define template-e
       (wrap-bindings new-idrs #`(#,(quote-syntax quasisyntax) #,new-template)))
     (cond
       [repetition? (make-repetition-info e
                                          #'template
                                          #`(pack-element* #,template-e #,depth)
                                          depth
                                          0
                                          (get-syntax-static-infos)
                                          #f)]
       [else (wrap-static-info* template-e
                                (get-syntax-static-infos))])]))

(define-for-syntax (convert-repetition-template e)
  (convert-template e #:repetition? #t))

;; optimization for `'$tail ...'`
(define-for-syntax (convert-direct-tail-template tail-id name)
  (define unpack (unwrap-static-infos
                  (repetition-as-list/unchecked #`(group #,tail-id) 1)))
  (wrap-static-info*
   (syntax-parse unpack
     #:datum-literals (unpack-tail-list*)
     [(unpack-tail-list* _ id 1) #'id]
     [_ #`(pack-tail (unpack-list-tail* (quote-syntax name) #,unpack 0))])
   (get-syntax-static-infos)))

(define-for-syntax (deepen-template-escapes idrs)
  (for/fold ([new-idrs null]
             [deepened #hasheq()]
             #:result new-idrs)
            ([idr (in-list (reverse idrs))])
    (syntax-parse idr
      #:literals (unpacking delaying dependent-unpack)
      [(id-pat (unpacking depth 0 . u))
       (values (cons #`[(id-pat (... ...)) (unpacking #,(add1 (syntax-e #'depth)) 0 . u)]
                     new-idrs)
               (hash-set deepened (extract-idr-name idr) #t))]
      [(id-pat (unpacking depth k . u))
       (values (cons #`[id-pat (unpacking depth #,(sub1 (syntax-e #'k)) . u)]
                     new-idrs)
               deepened)]
      [(id-pat (dependent-unpack ids (converter depth (qs t) . args)))
       (if (for/or ([id (in-list (syntax->list #'ids))])
             (hash-ref deepened (syntax-e id) #f))
           (values (cons #`[(id-pat (... ...))
                            (dependent-unpack ids (converter #,(add1 (syntax-e #'depth)) (qs (t (... ...))) . args))]
                         new-idrs)
                   (hash-set deepened (extract-idr-name idr) #t))
           (values (cons idr new-idrs)
                   deepened))]
      [(id-pat (converter depth (qs t) . args))
       (values (cons #`[(id-pat (... ...))
                        (converter #,(add1 (syntax-e #'depth)) (qs (t (... ...)))) . args]
                     new-idrs)
               (hash-set deepened (extract-idr-name idr) #t))])))

(define-for-syntax (extract-idr-name idr)
  (syntax-parse idr
    [(id-pat . _)
     (let extract ([id-pat #'id-pat])
       (syntax-parse id-pat
         [(id-pat . _) (extract #'id-pat)]
         [_ (syntax-e id-pat)]))]))

(define-for-syntax (adjust-template-sibling-depths idrs)
  ;; under `...`, so expose any unexposed repetitions
  (define u-idrs (expose-repetitions idrs))
  (cond
    [(or (null? u-idrs)
         (null? (cdr u-idrs)))
     u-idrs]
    [else
     (define max-depth
       (for/fold ([max-depth 0]) ([idr (in-list u-idrs)])
         (syntax-parse idr
           #:literals (unpacking)
           [[_ (unpacking depth k rep-info::repetition-info . _)]
            (max max-depth (- (syntax-e #'rep-info.bind-depth)
                              (syntax-e #'rep-info.use-depth)))]
           [_ max-depth])))
     (for/list ([idr (in-list u-idrs)])
       (syntax-parse idr
         #:literals (unpacking)
         [[pat (unpacking depth k rep-info::repetition-info . u)]
          #:do [(define want-depth (- (syntax-e #'rep-info.bind-depth)
                                      (syntax-e #'rep-info.use-depth)))]
          #:when (and (want-depth . < . max-depth)
                      ((syntax-e #'depth) . >= . want-depth))
          ;; ok to skip a `...` layer:
          #`[pat (unpacking depth 1 rep-info . u)]]
         [_ idr]))]))

(define-for-syntax (expose-repetitions idrs)
  (for/list ([idr (in-list idrs)])
    (syntax-parse idr
      #:literals (pending-unpack)
      [[id-pat (pending-unpack e . u)]
       ;; Since `e` is under `...`, it needs to parse as a repetition
       (syntax-parse #'(group e)
         [rep::repetition
          #`[id-pat (unpacking 0 0 rep.parsed . u)]])]
      [_ idr])))

(define-for-syntax (deepen-for-repetition idrs template)
  (define u-idrs (expose-repetitions idrs))
  (define depth
    (for/fold ([max-depth 0]) ([idr (in-list u-idrs)])
      (syntax-parse idr
        #:literals (unpacking)
        [[_ (unpacking depth k rep-info::repetition-info . _)]
         (max max-depth (- (- (syntax-e #'rep-info.bind-depth)
                              (syntax-e #'rep-info.use-depth))
                           (syntax-e #'depth)))]
        [_ max-depth])))
  (let loop ([d depth] [idrs u-idrs] [template template])
    (cond
      [(d . <= . 0) (values (max depth 0) idrs template)]
      [else (loop (sub1 d)
                  (deepen-template-escapes
                   (adjust-template-sibling-depths idrs))
                  #`(#,template (... ...)))])))

;; if we get here, it means that an escape was not under `...`
(define-syntax (pending-unpack stx)
  (syntax-parse stx
    [(_ e unpack* $-name) #'(unpack* $-name (rhombus-expression (group e)) 0)]))

(define-syntax (dependent-unpack stx)
  (syntax-parse stx
    [(_ ids u) #'u]))

;; if we get here, it means that an escape was under some number of `...`
(define-syntax (unpacking stx)
  (syntax-parse stx
    [(_ depth k rep-info::repetition-info unpack* $-name)
     (define base-e (repetition-as-list #'rep-info (syntax-e #'depth)))
     (define unpack*-id #'unpack*)
     (cond
       [(and (identifier? unpack*-id)
             (free-identifier=? unpack*-id #'unpack-tail*))
        (get-tail-repetition #'$-name base-e (sub1 (syntax-e #'depth))
                             #'unpack-tail-list* #'unpack-tail* #'unpack-list-tail*)]
       [(and (identifier? unpack*-id)
             (free-identifier=? unpack*-id #'unpack-multi-tail*))
        (get-tail-repetition #'$-name base-e (sub1 (syntax-e #'depth))
                             #'unpack-multi-tail-list* #'unpack-multi-tail* #'unpack-multi-list-tail*)]
       [else
        ;; strip away redundant unpack as an optimization
        (define opt-e
          (syntax-parse (unwrap-static-infos base-e)
            [(unpack*:id _ e _)
             #:when (free-identifier=? #'unpack* unpack*-id)
             #'e]
            [e
             #'e]))
        #`(unpack* $-name #,opt-e depth)])]))

(define-syntax (delaying stx)
  (syntax-parse stx
    [(_ depth . u)
     #'u]))

(define-for-syntax (get-tail-repetition $-name base-e depth-stx
                                        replaceable-unpack*-id replacement-unpack*-id generic-unpack*-id)
  ;; replace redundant unpack with alternative; this is not just an
  ;; optimization, but a change to the time complexity of using the
  ;; tail in a template by avoiding conversion to a list and back
  (syntax-parse (unwrap-static-infos base-e)
    [(unpack*:id $-name e _)
     #:when (free-identifier=? #'unpack* replaceable-unpack*-id)
     #`(#,replacement-unpack*-id $-name e #,depth-stx)]
    [e
     #`(#,generic-unpack*-id #,$-name e #,depth-stx)]))

(define-for-syntax (quoted-shape-dispatch stx in-space single-k group-k multi-k literal-k)
  (syntax-parse stx
    #:datum-literals (group)
    [(_ (_::quotes (group (~and special (~or* (~var _ (:... in-space)) (~var _ (:$ in-space)))))) . tail)
     (values (literal-k #'special)
             #'tail)]
    [(_ (_::quotes (group t)) . tail)
     (values (single-k #'t)
             #'tail)]
    [(_ (_::quotes g) . tail)
     #:when group-k
     (values (group-k #'g)
             #'tail)]
    [(_ (_::quotes . args) . tail)
     (values (multi-k (datum->syntax #f (cons (syntax-raw-property (datum->syntax #f 'multi) "")
                                              #'args)))
             #'tail)]))

(define-for-syntax ((convert-pattern/generate-match repack-id) e)
  (define-values (pattern idrs sidrs vars can-be-empty?) (convert-pattern e))
  (with-syntax ([((id id-ref) ...) idrs]
                [(((sid ...) sid-ref) ...) sidrs])
    (with-syntax ([(tmp-id ...) (generate-temporaries #'(id ...))])
      (binding-form
       #'syntax-infoer
       #`(#,(string-append "'" (shrubbery-syntax->string e) "'")
          #,pattern
          #,repack-id
          (tmp-id ...)
          (id ...)
          (id-ref ...)
          ((sid ...) ...)
          (sid-ref ...))))))

(define-syntax #%quotes
  (expression-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (quoted-shape-dispatch stx
                            in-expression-space
                            convert-template
                            #f
                            convert-template
                            (lambda (e) #`(quote-syntax #,e))))))

(define-binding-syntax #%quotes
  (binding-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (quoted-shape-dispatch stx
                            in-binding-space
                            (convert-pattern/generate-match #'repack-as-term)
                            #f
                            (convert-pattern/generate-match #'repack-as-multi)
                            (lambda (e) (binding-form
                                         #'syntax-infoer
                                         #`(#,(string-append "'" (shrubbery-syntax->string e) "'")
                                            #,(syntax-parse e
                                                [((~datum op) id) #`((~datum op) (~literal id))])
                                            repack-as-term
                                            ()
                                            ()
                                            ()
                                            ()
                                            ())))))))

(define-repetition-syntax #%quotes
  (repetition-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (quoted-shape-dispatch stx
                            in-expression-space
                            convert-repetition-template
                            #f
                            convert-repetition-template
                            (lambda (e) (make-repetition-info stx
                                                              #'template
                                                              #`(quote-syntax #,e)
                                                              0
                                                              0
                                                              #'()
                                                              #t))))))

(define-syntax syntax_term
  (expression-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group term)) . tail)
        (values (convert-template #'term) #'tail)]))))

(define-binding-syntax syntax_term
  (binding-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group term)) . tail)
        (values (convert-pattern/generate-match #'term) #'tail)]))))

(define-syntax (syntax-infoer stx)
  (syntax-parse stx
    [(_ static-infos (annotation-str pattern repack tmp-ids (id ...) id-refs (sids ...) sid-refs))
     (with-syntax ([(id-depth ...) (for/list ([id-ref (in-list (syntax->list #'id-refs))])
                                     (syntax-parse id-ref
                                       [(pack _ depth) #'depth]))]
                   [((sid sid-depth) ...)
                    (for/list ([sids (in-list (syntax->list #'(sids ...)))]
                               [sid-ref (in-list (syntax->list #'sid-refs))])
                      (extract-pattern-variable-bind-id-and-depth sids sid-ref))])
       (binding-info #'annotation-str
                     #'syntax
                     #'()
                     #'((id (id-depth)) ... (sid (sid-depth)) ...)
                     #'syntax-matcher
                     #'syntax-committer
                     #'syntax-binder
                     #'(pattern repack tmp-ids (id ...) id-refs (sids ...) sid-refs)))]))

(define-syntax (syntax-matcher stx)
  (syntax-parse stx
    [(_ arg-id (pattern repack (tmp-id ...) (id ...) (id-ref ...) (sid ...) (sid-ref ...)) IF success fail)
     #'(IF (syntax? arg-id)
           (begin
             (define-values (match? tmp-id ...)
               (syntax-parse (repack arg-id)
                 #:disable-colon-notation
                 [pattern (values #t id-ref ...)]
                 [_ (values #f 'id ...)]))
             (IF match?
                 success
                 fail))
           fail)]))

(define-syntax (syntax-committer stx)
  (syntax-parse stx
    [(_ arg-id (pattern repack (tmp-id ...) (id ...) (id-ref ...) (sid ...) (sid-ref ...)))
     #'(begin
         (define id tmp-id)
         ...)]))

(define-syntax (syntax-binder stx)
  (syntax-parse stx
    [(_ arg-id (pattern repack (tmp-id ...) (id ...) (id-ref ...) ((sid ...) ...) (sid-ref ...)))
     #'(begin
         (define-syntaxes (sid ...) sid-ref)
         ...)]))

(define-for-syntax (handle-syntax-parse-dispatch stx who in-expr binds-stx rhss-stx default-k)
  (define binds (syntax->list binds-stx))
  (cond
    [(for/and ([bind-stx (in-list binds)])
       (syntax-parse bind-stx
         [b::binding-form
          (free-identifier=? #'b.infoer-id #'syntax-infoer)]))
     ;; since all bindings are syntax patterns, generate a `syntax-parse` form
     ;; without an "else" case, so it synthesize a message when matching fails
     (define repack-multi?
       (for/or ([bind (in-list binds)])
         (syntax-parse bind
           [b::binding-form
            (syntax-parse #'b.data
              [(annotation-str pattern repack . _)
               (free-identifier=? #'repack #'repack-as-multi)])])))
     (relocate+reraw
      (respan stx)
      #`(syntax-parse (#,(if repack-multi? #'repack-as-multi #'repack-as-term) #,in-expr)
          #:disable-colon-notation
          #:context '#,who
          #,@(for/list ([bind (in-list binds)]
                        [rhs (in-list (syntax->list rhss-stx))])
               (syntax-parse bind
                 [b::binding-form
                  #:with b-impl::binding-impl #'(b.infoer-id () b.data)
                  (syntax-parse #'b-impl.info
                    [b::binding-info
                     (syntax-parse #'b.data
                       [(pattern repack (tmp-id ...) (id ...) (id-ref ...) ((sid ...) ...) (sid-ref ...))
                        #`[#,(if (and repack-multi?
                                      (free-identifier=? #'repack #'repack-as-term))
                                 #`((~datum multi) ((~datum group) pattern))
                                 #'pattern)
                           (define id id-ref) ...
                           (define-syntaxes (sid ...) sid-ref)
                           ...
                           #,(syntax-parse rhs
                               [(block-tag g ...)
                                #'(rhombus-body-at block-tag g ...)])]])])]))))]
    [else (default-k)]))
