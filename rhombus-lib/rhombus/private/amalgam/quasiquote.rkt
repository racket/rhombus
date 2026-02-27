#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     shrubbery/property
                     enforest/hier-name-parse
                     "srcloc.rkt"
                     "tag.rkt"
                     "name-path-op.rkt"
                     "origin.rkt")
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
         "sequence-pattern.rkt"
         "syntax-wrap.rkt"
         "dotted-sequence-parse.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt")

(provide (for-spaces (#f
                      rhombus/bind
                      rhombus/repet
                      rhombus/unquote_bind)
                     #%quotes)
         (for-space rhombus/unquote_bind
                    _))

(module+ for-match-ns
 (provide (for-space rhombus/unquote_bind
                     cut)))

(module+ convert
  (begin-for-syntax
    (provide convert-pattern
             convert-template)))

(module+ shape-dispatch
  (provide (for-syntax quoted-shape-dispatch)))

(module+ for-match
  (provide (for-syntax handle-syntax-parse-dispatch)))

(begin-for-syntax
  (define-splicing-syntax-class (:list-repetition in-space repetition-mode?)
    #:attributes (name mode)
    #:datum-literals (group)
    (pattern (~seq (~var || (:... in-space)) #:nonempty)
             #:when repetition-mode?
             #:attr mode 'nonempty)
    (pattern (~seq (~var || (:... in-space)) #:once)
             #:when repetition-mode?
             #:attr mode 'once)
    (pattern (~seq (~var || (:... in-space)))
             #:attr mode #f)
    (pattern (~seq (group (~var || (:... in-space)) #:nonempty))
             #:when repetition-mode?
             #:attr mode 'nonempty)
    (pattern (~seq (group (~var || (:... in-space)) #:once))
             #:when repetition-mode?
             #:attr mode 'once)
    (pattern (~seq (group (~var || (:... in-space))))
             #:attr mode #f)
    (pattern (~seq (_::block (group (~var || (:... in-space))) #:nonempty))
             #:when repetition-mode?
             #:attr mode 'nonempty)
    (pattern (~seq (_::block (group (~var || (:... in-space))) #:once))
             #:when repetition-mode?
             #:attr mode 'once)
    (pattern (~seq (_::block (group (~var || (:... in-space)))))
             #:attr mode #f))
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
  (define-splicing-syntax-class (:cut dotted?)
    #:datum-literals (op)
    (pattern seq::dotted-operator-or-identifier-sequence
             #:when (not dotted?)
             #:with (~var name (:hier-name-seq in-name-root-space in-unquote-binding-space name-path-op name-root-ref)) #'seq
             #:when (free-identifier=? (in-unquote-binding-space #'name.name)
                                       (unquote-bind-quote cut)))
    (pattern (_::parens (group (~var _ (:cut dotted?))))))
  (define-splicing-syntax-class (:tail-repetition in-space dotted?)
    #:attributes (name term)
    (pattern (~seq (~var _ (:$ in-space)) (~var || (:esc dotted? #f)) (~var || (:... in-space)))))
  (define-splicing-syntax-class (:block-tail-repetition in-space dotted?)
    #:attributes (name term)
    #:datum-literals (group)
    (pattern (~seq (group (~var _ (:$ in-space)) (~var || (:esc dotted? #f)))
                   (group (~var || (:... in-space))))))

  (define (replace-...1 ps)
    (syntax-parse ps
      #:literals (...1)
      [(a ...1 . tail)
       #`((~and (~or (~seq _) (~seq)) (~seq a (... ...)))
          . #,(replace-...1 #'tail))]
      [(a . tail)
       (define old-t #'tail)
       (define new-t (replace-...1 old-t))
       (if (eq? old-t new-t)
           ps
           #`(a . #,new-t))]
      [_ ps]))

  (struct consumes-outer (p) #:prefab))

(define ...1 (void))

(define-for-syntax (convert-syntax orig-e make-datum make-literal make-void
                                   handle-escape handle-group-escape handle-multi-escape
                                   adjust-escape-siblings deepen-escapes deepen-syntax-escape
                                   handle-tail-escape handle-block-tail-escape
                                   handle-maybe-empty-sole-group
                                   handle-maybe-empty-alts handle-maybe-misformed-group
                                   #:handle-midgroups-multi-escape [handle-midgroups-multi-escape #f]
                                   #:in-space in-space
                                   #:tail-any-escape? [tail-any-escape? #f]
                                   #:as-tail? [as-tail? #f]
                                   #:splice? [splice? #f]
                                   #:splice-pattern [splice-pattern #f]
                                   #:allow-fltten? [allow-flatten? #f]
                                   #:repetition-mode? [repetition-mode? #f]
                                   #:make-describe-op [make-describe-op (lambda (e name) e)]
                                   #:improve-repetition-constraints [improve-repetition-constraints (lambda (ps gs) ps)])
  (let convert ([e orig-e] [empty-ok? splice?] [as-tail? as-tail?] [splice? splice?] [outer-gs #f] [handle-gs #f] [after-block? #f])
    (syntax-parse e
      #:datum-literals (group parens brackets braces block quotes multi alts)
      ;; `$esc` as a group at the non-tail of a group sequence
      [(group
        (~var $-id (:$ in-space)) (~var esc (:esc tail-any-escape? #f)))
       #:when (and (not as-tail?) (not splice?))
       ;; Special case: a group whose content is an escape; the escape
       ;; defaults to "group" mode instead of "term" mode, and if `outer-gs`
       ;; has no escapes, then "multi" mode is an option
       #:do [(define-values (p1 new-idrs1 new-sidrs1 new-vars1)
               (if (and handle-midgroups-multi-escape
                        outer-gs
                        (for/and ([n-g (in-list (syntax->list outer-gs))])
                          (syntax-parse n-g
                            [(_ (~var _ (:$ in-space)) . _) #f]
                            [(_ (~var _ (:... in-space)) . _) #f]
                            [_ #t])))
                   (handle-midgroups-multi-escape #'$-id.name #'esc.term e outer-gs handle-gs)
                   (values #f #f #f #f)))
             (define-values (p new-idrs new-sidrs new-vars)
               (if p1
                   (values p1 new-idrs1 new-sidrs1 new-vars1)
                   (handle-group-escape #'$-id.name #'esc.term e)))]
       #:when p
       (values (if p1 (consumes-outer p) p) new-idrs new-sidrs new-vars #f)]
      ;; `$esc` as a group in parens, etc. => maybe a multi-group splice
      [((~or* quotes multi block) ;; not: parens, brackets, braces
        (group (~var $-id (:$ in-space)) (~var esc (:esc tail-any-escape? #f))))
       #:when (and (not as-tail?))
       ;; Analogous special case, but for blocks (maybe within an `alts`), etc.
       #:do [(define-values (p new-idrs new-sidrs new-vars) (handle-multi-escape #'$-id.name #'esc.term e splice?))]
       #:when p
       (values p new-idrs new-sidrs new-vars #f)]
      ;; single group with parens, etc. => special consruction case to support empty group as empty sequence
      [((~and tag (~or* parens brackets braces quotes multi block))
        (~and g (group . _)))
       #:when (not splice?)
       ;; Special case: for a single group with (), [], {}, '', or block, if the group
       ;; can be empty, allow a match/construction with zero groups
       (define-values (p new-idrs new-sidrs new-vars can-be-empty?) (convert #'g #t as-tail? #f #'() #f #f))
       (if can-be-empty?
           (handle-maybe-empty-sole-group #'tag p new-idrs new-sidrs new-vars)
           (values (no-srcloc #`(#,(make-datum #'tag) #,@(if (consumes-outer? p)
                                                             (consumes-outer-p p)
                                                             (list p))))
                   new-idrs
                   new-sidrs
                   new-vars
                   #f))]
      [((~and tag (~or* parens brackets braces quotes multi block alts group))
        g ...)
       (let loop ([gs #'(g ...)] [pend-idrs #f] [pend-sidrs #f] [pend-vars #f]
                                 [idrs '()]  ; list of #`[#,id #,rhs #,statinfo ...] for definitions
                                 [sidrs '()] ; list of #`[(#,id ...) #,rhs #,statinfo ...] for syntax definitions
                                 [vars '()]  ; list of `[,id . ,depth] for visible subset of `idrs` and `sidrs`
                                 [ps '()] [can-be-empty? #t] [pend-is-splice? #f] [tail #f]
                                 [needs-group-check? #f]
                                 [splice? splice?]
                                 [just-after-block? #f])
         (define really-can-be-empty? (and can-be-empty? (or pend-is-splice? (not pend-idrs))))
         (define (simple gs)
           (syntax-parse gs
             [(g . gs)
              (define next-after-block? (syntax-parse #'g
                                          #:datum-literals (block)
                                          [(block . _) #t]
                                          [_ #f]))
              (define-values (p/tail new-ids new-sidrs new-vars nested-can-be-empty?)
                (convert #'g #f #f #f #'gs (lambda ()
                                             (loop #'gs null null null
                                                   null
                                                   null
                                                   null
                                                   null #f #f #f
                                                   #f
                                                   #f
                                                   next-after-block?))
                         just-after-block?))
              (cond
                [(consumes-outer? p/tail)
                 (define tail (consumes-outer-p p/tail))
                 (finish (reverse ps) tail
                         (append new-ids (or pend-idrs '()) idrs)
                         (append new-sidrs (or pend-sidrs '()) sidrs)
                         (append new-vars (or pend-vars '()) vars)
                         #f #f)]
                [else
                 (loop #'gs new-ids new-sidrs new-vars
                       (append (or pend-idrs '()) idrs)
                       (append (or pend-sidrs '()) sidrs)
                       (append (or pend-vars '()) vars)
                       (cons p/tail ps) really-can-be-empty? #f #f
                       needs-group-check?
                       splice?
                       next-after-block?)])]))
         (define (finish ps tail idrs sidrs vars can-be-empty? needs-group-check?)
           (define (ps+tail) (let ([ps (if tail (append ps tail) ps)])
                               (if repetition-mode?
                                   (replace-...1 ps)
                                   ps)))
           (cond
             [(and can-be-empty? (eq? (syntax-e #'tag) 'alts))
              (handle-maybe-empty-alts #'tag (ps+tail) idrs sidrs vars after-block?)]
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
           #:datum-literals (op group)
           [()
            (finish (reverse ps) tail
                    (append (or pend-idrs '()) idrs)
                    (append (or pend-sidrs '()) sidrs)
                    (append (or pend-vars '()) vars)
                    really-can-be-empty?
                    needs-group-check?)]
           ;; `$var ...` at end of a sequence (of terms or groups) => tail repetition
           [((~var op (:tail-repetition in-space tail-any-escape?)))
            #:when (and (or tail-any-escape?
                            (identifier? #'op.term))
                        (or (not splice?)
                            as-tail?))
            (define-values (id new-idrs new-sidrs new-vars) (handle-tail-escape #'op.name #'op.term e))
            (loop #'() #f #f #f
                  (append new-idrs (or pend-idrs '()) idrs)
                  (append new-sidrs (or pend-sidrs '()) sidrs)
                  (append new-vars (or pend-vars '()) vars)
                  ps really-can-be-empty? #f id
                  ;; tail escape is responsible for making sure it's valid
                  needs-group-check?
                  splice?
                  #f)]
           ;; `$var ...` as a whole group within a sequence of groups => block tail repetition
           [((~var op (:block-tail-repetition in-space tail-any-escape?)))
            #:when (or tail-any-escape?
                       (identifier? #'op.term))
            (define-values (id new-idrs new-sidrs new-vars) (handle-block-tail-escape #'op.name #'op.term e))
            (loop #'() #f #f #f
                  (append new-idrs (or pend-idrs '()) idrs)
                  (append new-sidrs (or pend-sidrs '()) sidrs)
                  (append new-vars (or pend-vars '()) vars)
                  ps really-can-be-empty? #f id
                  #f
                  splice?
                  #f)]
           ;; `$var ...` not at the end of a sequence (of terms or groups)
           [((~var op (:list-repetition in-space repetition-mode?)) . gs)
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
            (define dots (case (attribute op.mode)
                           [(nonempty) (quote-syntax ...+)]
                           [(once) (quote-syntax ...1)]
                           [else (quote-syntax ...)]))
            (if allow-flatten?
                (loop #'gs new-pend-idrs new-pend-sidrs new-pend-vars
                      idrs
                      sidrs
                      vars
                      (cons dots ps) can-be-empty? #t #f
                      #t
                      splice?
                      #f)
                (let ([ps (if (eq? (syntax-e #'tag) 'group)
                              (improve-repetition-constraints ps #'gs)
                              ps)])
                  (loop #'gs #f #f #f
                        (append new-pend-idrs idrs)
                        (append new-pend-sidrs sidrs)
                        (append new-pend-vars vars)
                        (cons dots ps) can-be-empty? #f #f
                        #t
                        splice?
                        #f)))]
           ;; `$ cut` within a sequence
           [((op (~var $-id (:$ in-space))) (~var _ (:cut tail-any-escape?)) . gs)
            (loop #'gs #f #f #f
                  (append (or pend-idrs '()) idrs)
                  (append (or pend-sidrs '()) sidrs)
                  (append (or pend-vars '()) vars)
                  (cons #'~! ps) really-can-be-empty? #f #f
                  needs-group-check?
                  splice?
                  #f)]
           ;; `$esc` within a sequence
           [((op (~var $-id (:$ in-space))) (~var esc (:esc tail-any-escape? #t)) . n-gs)
            (define could-tail? (or (and (not tail) (not splice?)) as-tail?))
            (define tail? (and (null? (syntax-e #'n-gs))
                               could-tail?))
            (define fixed-terms-after-gs (and could-tail?
                                              ;; `$() must be at end
                                              (syntax-parse #'esc
                                                [((_::parens)) #f]
                                                [_ #t])
                                              (let* ([n-gs (syntax->list #'n-gs)]
                                                     [n-gs (if (and (pair? n-gs)
                                                                    (pair? (cdr n-gs)))
                                                               ;; allow `$()` after a group
                                                               (let ([rev-n-gs (reverse n-gs)])
                                                                 (syntax-parse (list (cadr rev-n-gs)
                                                                                     (car rev-n-gs))
                                                                   [((~var _ (:$ in-space)) (_::parens))
                                                                    (reverse (cddr rev-n-gs))]
                                                                   [_ n-gs]))
                                                               n-gs)])
                                                (and (for/and ([n-g (in-list n-gs)])
                                                       (syntax-parse n-g
                                                         [(~var _ (:$ in-space)) #f]
                                                         [(~var _ (:... in-space)) #f]
                                                         [_ #t]))
                                                     (datum->syntax #f n-gs)))))
            (define-values (pat new-idrs new-sidrs new-vars pat-as-tail?)
              (handle-escape #'$-id.name #'esc.term e tail?
                             (or fixed-terms-after-gs #'())
                             (and fixed-terms-after-gs
                                  (lambda ()
                                    (loop fixed-terms-after-gs '() '() '()
                                          '()
                                          '()
                                          '()
                                          null
                                          #t #f #f
                                          #t
                                          #f
                                          #f)))))
            (cond
              [pat-as-tail?
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
                     really-can-be-empty? #t #f
                     #t
                     splice?
                     #f)])]
           ;; `$` with nothing afterward
           [((op (~var $-id (:$ in-space))))
            (raise-syntax-error #f
                                "misplaced escape"
                                #'$-id.name)]
           ;; normal case
           [(g . _)
            (simple gs)]))]
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
  (define (add-escape-origin $-id stx)
    (syntax-property stx 'origin (cons (in-binding-space
                                        (syntax-local-introduce $-id))
                                       (or (syntax-property stx 'origin) null))))
  (define (handle-escape $-id e in-e ctx-kind)
    (define parsed
      (syntax-parse #`(#,group-tag #,e)
        [(~var esc (:unquote-binding ctx-kind)) #'esc.parsed]))
    (define (track stx)
      (syntax-parse stx
        #:datum-literals (~var ~seq)
        [((~and var-tag ~var) id . rest)
         #`(var-tag #,(track #'id) . rest)]
        [((~and seq-tag ~seq) pat . rest)
         #`(seq-tag #,(track #'pat) . rest)]
        [_ (add-escape-origin $-id (transfer-origin parsed stx))]))
    (syntax-parse parsed
      [#f (values #f #f #f #f)]
      [id:identifier
       (if (eq? ctx-kind 'grouplet)
           (values #f #f #f #f)
           (identifier-as-unquote-binding (track #'id) ctx-kind
                                          #:result (lambda (kind . rest) (apply values rest))
                                          #:pattern-variable pattern-variable))]
      [(kind pat idrs sidrs vars)
       (values (track #'pat)
               (syntax->list #'idrs)
               (syntax->list #'sidrs)
               (for/list ([var (in-list (syntax->list #'vars))])
                 (syntax-list->pattern-variable var)))]))
  (define (handle-escape/match-head $-id e in-e kind splice?)
    (define-values (p idrs sidrs vars) (handle-escape $-id e in-e kind))
    (if p
        (values (cond
                  [splice?
                   ;; splicing `multi` means match any head
                   p]
                  [else
                   (syntax-parse in-e
                     [(tag . _)
                      (cond
                        [(eq? 'group (syntax-e #'tag))
                         ;; no check needed, and might use `~seq` to splice itself
                         p]
                        [else
                         #`(~and (#,(make-datum #'tag) . _) #,p)])])])
                idrs
                sidrs
                vars)
        (values #f #f #f #f)))
  (convert-syntax e
                  #:in-space in-binding-space
                  #:as-tail? as-tail?
                  #:splice? splice?
                  #:splice-pattern splice-pattern
                  #:repetition-mode? #t
                  ;; make-datum
                  make-datum
                  ;; make-literal
                  (lambda (d)
                    #`(~datum #,d))
                  ;; make-void
                  (lambda (e)
                    #`(~datum #,(void)))
                  ;; handle-escape:
                  (lambda ($-id e in-e tail? fixed-tail-gs handle-fixed-tail)
                    (let-values ([(p new-idrs new-sidrs new-vars)
                                  (if (or tail? handle-fixed-tail)
                                      (handle-escape $-id e in-e 'grouplet)
                                      (values #f #f #f #f))])
                      (if p
                          (with-syntax ([(tmp) (generate-temporaries '(tail))])
                            (cond
                              [(null? (syntax-e fixed-tail-gs))
                               (values #`(~and tmp (~parse #,p (cons group-tag #'tmp)))
                                       new-idrs new-sidrs new-vars #t)]
                              [else
                               ;; there are term patterns after this group position
                               (define-values (tail-p tail-idrs tail-sidrs tail-vars tail-empty?)
                                 (handle-fixed-tail))
                               (with-syntax ([(tail-tmp ...) (generate-temporaries fixed-tail-gs)])
                                 (values #`(~and (tmp (... ...+) tail-tmp ...)
                                                 (~parse #,p (cons group-tag #'(tmp (... ...))))
                                                 (~parse #,(cdr (syntax-e tail-p)) #'(tail-tmp ...)))
                                         (append new-idrs tail-idrs)
                                         (append new-sidrs tail-sidrs)
                                         (append new-vars tail-vars)
                                         #t))]))
                          (let-values ([(p new-idrs new-sidrs new-vars) (handle-escape $-id e in-e 'term)])
                            (if p
                                (values (if tail? (list p) p) new-idrs new-sidrs new-vars tail?)
                                (raise-syntax-error #f
                                                    "incompatible with this context"
                                                    e))))))
                  ;; handle-group-escape:
                  (lambda ($-id e in-e)
                    (handle-escape/match-head $-id e in-e 'group #f))
                  ;; handle-multi-escape:
                  (lambda ($-id e in-e splice?)
                    (define kind
                      (syntax-parse in-e
                        [(head . _) (if (eq? (syntax-e #'head) 'block)
                                        'block
                                        'multi)]
                        [_ 'multi]))
                    (handle-escape/match-head $-id e in-e kind splice?))
                  #:handle-midgroups-multi-escape
                  (lambda ($-id e in-e fixed-tail-gs handle-fixed-tail)
                    (let-values ([(p new-idrs new-sidrs new-vars) (handle-escape $-id e in-e 'group)])
                      (cond
                        [p
                         (with-syntax ([(tmp) (generate-temporaries '(tail))])
                           (cond
                             [(null? (syntax-e fixed-tail-gs))
                              (values #`(#,p) new-idrs new-sidrs new-vars)]
                             [else
                              ;; there are group patterns after this one
                              (define-values (tail-p tail-idrs tail-sidrs tail-vars tail-empty?)
                                (handle-fixed-tail))
                              (with-syntax ([(tail-tmp ...) (generate-temporaries fixed-tail-gs)])
                                (values #`(~and (tmp (... ...) tail-tmp ...)
                                                (~parse (#,p) #'(tmp (... ...)))
                                                (~parse #,(cdr (syntax-e tail-p)) #'(tail-tmp ...)))
                                        (append new-idrs tail-idrs)
                                        (append new-sidrs tail-sidrs)
                                        (append new-vars tail-vars)))]))]
                        [else (values #f #f #f #f)])))
                  ;; adjust-escape-siblings
                  (lambda (idrs)
                    idrs)
                  ;; deepen-escapes
                  (lambda (idrs)
                    (for/list ([idr (in-list idrs)])
                      (syntax-parse idr
                        #:literals (syntax)
                        #:datum-literals (maybe-syntax-wrap)
                        [(id (pack (syntax stx) depth))
                         #`(id (pack (syntax (stx (... ...))) #,(add1 (syntax-e #'depth))))]
                        [(id ((~and msw maybe-syntax-wrap) (pack (syntax stx) depth) m-depth . msw-tail))
                         #`(id (msw (pack (syntax (stx (... ...))) #,(add1 (syntax-e #'depth))) #,(add1 (syntax-e #'m-depth)) . msw-tail))]
                        ;; If not `syntax`, then something like `attribute`, which doesn't need deepening
                        [(id (pack-nothing* val depth))
                         #`(id (pack-nothing* val #,(add1 (syntax-e #'depth))))]
                        [(id ((~and msw maybe-syntax-wrap) (pack-nothing* val depth) m-depth . msw-tail))
                         #`(id (msw (pack-nothing* val #,(add1 (syntax-e #'depth))) #,(add1 (syntax-e #'m-depth)) . msw-tail))])))
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
                                 (list (make-pattern-variable-bind e temp-id (quote-syntax unpack-tail-list*) 1))
                                 (list (pattern-variable (syntax-e e) e temp-id 1 (quote-syntax unpack-tail-list*) 'stx))))]))
                  ;; handle-block-tail-escape:
                  (lambda (name e in-e)
                    (syntax-parse e
                      [_::_-bind
                       (values #'_ null null null)]
                      [else
                       (let ([temp0-id (car (generate-temporaries (list e)))]
                             [temp-id (car (generate-temporaries (list e)))])
                         (values temp0-id
                                 (list #`[#,temp-id (pack-multi-tail* (syntax #,temp0-id) 0)])
                                 (list (make-pattern-variable-bind e temp-id (quote-syntax unpack-multi-tail-list*) 1))
                                 (list (pattern-variable (syntax-e e) e temp-id 1 (quote-syntax unpack-multi-tail-list*) 'stx))))]))
                  ;; handle-maybe-empty-sole-group
                  (lambda (tag pat idrs sidrs vars)
                    ;; `pat` matches a `group` form that's supposed to be under `tag`,
                    ;; but if `pat` match `(group)`, then allow an overall match to `(tag)`
                    (values #`(~or* (#,(make-datum tag) #,pat)
                                    (~and (#,(make-datum tag))
                                          ;; sets all pattern variables to nested empties:
                                          (_ . #,(syntax-parse pat
                                                   [(_ . tail) #'tail]))))
                            idrs
                            sidrs
                            vars
                            #f))
                  ;; handle-maybe-empty-alts
                  (lambda (tag ps idrs sidrs vars just-aftre-block?)
                    ;; if `(tag . ps)` would match `(alts)`, then let it match `(block)`
                    (values #`(~or* (#,(make-datum tag) . #,ps)
                                    (~and ((~datum block))
                                          ;; sets all pattern variables to nested empties:
                                          (_ . #,ps)))
                            idrs
                            sidrs
                            vars
                            #t))
                  ;; handle-maybe-misformed-group
                  (lambda (tag ps tail idrs sidrs vars can-be-empty? empty-ok?)
                    (let ([ps (replace-...1 (if tail (append ps tail) ps))])
                      ;; the `(tag . ps)` could match `(group)` or an otherwise misformed group,
                      ;; but that shouldn't be an input
                      (values #`(#,(make-datum tag) . #,ps) idrs sidrs vars #t)))
                  #:make-describe-op
                  (lambda (e name)
                    (describe-op-pattern e name))
                  #:improve-repetition-constraints
                  (lambda (ps gs)
                    ;; The first pattern in `ps` is followed by `...`.
                    ;; If the last pattern in `gs` matches `block` or `alts`, then we
                    ;; may know that repetition matches can't be `block` or `alts`
                    ;; terms, and specifying that constraint up front can avoid
                    ;; "expected more terms" messages where more terms are not
                    ;; possible. For example, when matching '$a ...: 1', a block
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
                        [(_ . gs) (loop #'gs)]
                        ;; the last pattern may match 0 terms, so be conservative
                        [_ (no-wrap)])))))

(define-unquote-binding-syntax #%quotes
  (unquote-binding-prefix-operator
   #f
   null
   'macro
   (lambda (stx ctx-kind)
     (syntax-parse stx
       [(form-id qs . tail)
        (define ((build pat-kind) e)
          (cond
            [(and (eq? pat-kind 'term)
                  (not (eq? ctx-kind 'term)))
             #'#f]
            [(and (eq? ctx-kind 'grouplet)
                  (eq? pat-kind 'multi)
                  (syntax-parse e [(_) #t] [_ #f]))
             ;; defer special case to term context
             #'#f]
            [(and (eq? ctx-kind 'block)
                  (eq? pat-kind 'group))
             ;; request to the context to again in 'group mode:
             #'#f]
            [else
             (define-values (use-e splice?)
               (cond
                 [(and (eq? pat-kind 'multi)
                       (memq ctx-kind '(term grouplet)))
                  ;; empty multi-group term is a special case that we can splice
                  ;; into a term context
                  (syntax-parse e
                    [(_) (values #'(group) #t)]
                    [_ (raise-syntax-error #f
                                           (format "multi-group pattern incompatible with ~a context"
                                                   (case ctx-kind
                                                     [(term) "term"]
                                                     [(grouplet) "group"]))
                                           #'qs)])]
                 [else (values e (and (eq? pat-kind 'group)
                                      (eq? ctx-kind 'term)))]))
             (define-values (pattern idrs sidrs vars can-be-empty?)
               (convert-pattern use-e
                                #:splice? splice?))
             (define pattern*
               (cond
                 [(and (not splice?) (eq? pat-kind 'multi))
                  (syntax-parse pattern
                    [(_ . tail)
                     (if (eq? ctx-kind 'group)
                         ;; turn into a splicing pattern
                         #`(~seq . tail)
                         ;; replace literal `multi` head with a wildcard, and the context
                         ;; will add a constraint for the right head symbol as needed
                         #`(_ . tail))])]
                 [(and (eq? pat-kind 'group) (eq? ctx-kind 'multi))
                  #`(_ #,pattern)]
                 [(and (eq? pat-kind 'group) (eq? ctx-kind 'block))
                  #`((~datum block) #,pattern)]
                 [else pattern]))
             (relocate+reraw
              stx
              #`(#,ctx-kind #,pattern* #,idrs #,sidrs #,(map pattern-variable->list vars)))]))
        (define-values (r empty-tail)
          (quoted-shape-dispatch #'(form-id qs)
                                 in-binding-space
                                 (build 'term)
                                 (build 'group)
                                 (build 'multi)
                                 (lambda (e)
                                   (cond
                                     [(eq? ctx-kind 'term)
                                      (define pat (syntax-parse e
                                                    #:datum-literals (op)
                                                    [((~and tag op) op-name)
                                                     (describe-op-pattern (no-srcloc #`((~datum tag) (~datum op-name)))
                                                                          #'op-name)]
                                                    [_ #`(~datum #,e)]))
                                      #`(#,ctx-kind #,pat () () ())]
                                     [else
                                      #'#f]))))
        (values r #'tail)]))))

(begin-for-syntax
  (define (describe-op-pattern e name)
    #`(~describe #:opaque
                 #,(format "the operator `~a`"
                           (or (syntax-raw-property name)
                               (syntax-e name)))
                 #,e)))

(define-unquote-binding-syntax _
  (unquote-binding-transformer
   (lambda (stx ctx-kind)
     (syntax-parse stx
       [(form-id . tail)
        (values #`(#,ctx-kind #,(syntax/loc #'form-id _) () () ())
                #'tail)]))))

(define-unquote-binding-syntax cut
  (unquote-binding-transformer
   (lambda (stx ctx-kind)
     (syntax-parse stx
       [(self . tail)
        (raise-syntax-error #f "incompatible with this context" #'self)]))))

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
                       (lambda ($-id e in-e tail? fixed-tail-gs handle-fixed-tail)
                         (check-escape e)
                         (define id (car (generate-temporaries (list e))))
                         (values (if tail? id #`(#,(quote-syntax ~@) . #,id))
                                 (list #`[#,id (pending-unpack #,e unpack-term-list* (quote-syntax #,$-id))]) null null
                                 tail?))
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
                       (lambda (tag ts idrs sidrs vars after-block?)
                         ;; if `(tag . ts)` generates `(alts)`, then produce `(block)` instead
                         ;; --- unless this `alts` is after a block, in which case produce ``
                         (define id (car (generate-temporaries '(alts))))
                         (values (if after-block?
                                     (no-srcloc #`(#,(quote-syntax ~@) #,id (... ...)))
                                     id)
                                 (cons #`[#,(if after-block? #`(#,id (... ...)) id)
                                          (convert-empty-alts 0 (#,(quote-syntax quasisyntax) #,(no-srcloc #`(#,tag . #,ts)))
                                                              #,after-block?)]
                                       idrs)
                                 sidrs
                                 vars
                                 #f))
                       ;; handle-maybe-misformed-group
                       (lambda (tag ts tail idrs sidrs vars can-be-empty? empty-ok?)
                         ;; Need to check that `ts` followed by an optional tail is well formed
                         ;; (e.g., no alnternatives in the middle of the group); the `tail` is
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

     (define depth (if repetition?
                       (get-repetition-depth idrs template)
                       0))
     (define (wrap-bindings idrs body)
       (cond
         [(null? idrs) body]
         [else
          (wrap-bindings (cdr idrs)
                         (syntax-parse (car idrs)
                           [(id-pat e)
                            #`(with-syntax ([id-pat e])
                                #,body)]))]))
     (cond
       [repetition?
        (define new-idrs (adjust-template-sibling-depths idrs))
        (define template-e
          (wrap-bindings (unwrap-template-repetitions new-idrs depth)
                         #`(#,(quote-syntax quasisyntax) #,template)))
        (make-repetition-info (list e)
                              (for/list ([i (in-range depth)])
                                (template-repetition-bindings new-idrs (- depth i 1)))
                              #`(pack-element* #,template-e 0)
                              (get-syntax-static-infos)
                              0)]
        [else
         (define template-e
           (wrap-bindings idrs #`(#,(quote-syntax quasisyntax) #,template)))
         (wrap-static-info* template-e
                            (get-syntax-static-infos))])]))

(define-for-syntax (convert-repetition-template e)
  (convert-template e #:repetition? #t))

;; optimization for `'$tail ...'`
(define-for-syntax (convert-direct-tail-template tail-id name)
  (define unpack (unwrap-static-infos
                  (syntax-parse #`(group #,tail-id)
                    [rep::repetition
                     (render-repetition #'for/list #'rep.parsed)])))
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
                        (converter #,(add1 (syntax-e #'depth)) (qs (t (... ...))) . args)]
                     new-idrs)
               (hash-set deepened (extract-idr-name idr) #t))])))

(define-for-syntax (unwrap-template-repetitions idrs overall-depth)
  (for/list ([idr (in-list idrs)])
    (syntax-parse idr
      #:literals (unpacking delaying dependent-unpack)
      [(id-pat (unpacking depth k rep-info::repetition-info unpack* $-name))
       (define i-depth (- (length (syntax->list #'rep-info.for-clausess))
                          (syntax-e #'depth)))
       (with-syntax ([new-rep (unwrap-repetition #'rep-info i-depth)])
         #`[id-pat (unpacking depth k new-rep unpack* $-name)])]
      [_ idr])))

(define-for-syntax (template-repetition-bindings idrs i)
  (apply
   append
   (for/list ([idr (in-list idrs)])
     (syntax-parse idr
       #:literals (unpacking delaying dependent-unpack)
       [(id-pat (unpacking depth k rep-info::repetition-info . u))
        (define i-depth (- (length (syntax->list #'rep-info.for-clausess))
                           (syntax-e #'depth)))
        (if (i-depth . > . i)
            (syntax->list (list-ref (syntax->list #'rep-info.for-clausess)
                                    (- i-depth i 1)))
            null)]
       [_
        null]))))

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
            (max max-depth (length (syntax->list #'rep-info.for-clausess)))]
           [_ max-depth])))
     (for/list ([idr (in-list u-idrs)])
       (syntax-parse idr
         #:literals (unpacking)
         [[pat (unpacking depth k rep-info::repetition-info . u)]
          #:do [(define want-depth (length (syntax->list #'rep-info.for-clausess)))]
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

(define-for-syntax (get-repetition-depth idrs template)
  (define u-idrs (expose-repetitions idrs))
  (define depth
    (for/fold ([max-depth 0]) ([idr (in-list u-idrs)])
      (syntax-parse idr
        #:literals (unpacking)
        [[_ (unpacking depth k rep-info::repetition-info . _)]
         (max max-depth (- (length (syntax->list #'rep-info.for-clausess))
                           (syntax-e #'depth)))]
        [_ max-depth])))
  (max depth 0))

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
     (define base-e (repetition-as-nested-lists #'rep-info (syntax-e #'depth) #'for/list))
     (define unpack*-id #'unpack*)
     (cond
       [(and (identifier? unpack*-id)
             (free-identifier=? unpack*-id #'unpack-tail*))
        (get-tail-repetition #'$-name base-e (sub1 (syntax-e #'depth))
                             #'unpack-tail-list* #'unpack-tail* #'unpack-list-tail*
                             #'rep-info)]
       [(and (identifier? unpack*-id)
             (free-identifier=? unpack*-id #'unpack-multi-tail*))
        (get-tail-repetition #'$-name base-e (sub1 (syntax-e #'depth))
                             #'unpack-multi-tail-list* #'unpack-multi-tail* #'unpack-multi-list-tail*                             
                             #'rep-info)]
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
                                        replaceable-unpack*-id replacement-unpack*-id generic-unpack*-id
                                        orig-rep)
  (transfer-origin
   orig-rep
   ;; replace redundant unpack with alternative; this is not just an
   ;; optimization, but a change to the time complexity of using the
   ;; tail in a template by avoiding conversion to a list and back
   (syntax-parse (unwrap-static-infos base-e)
     [(unpack*:id $-name e _)
      #:when (free-identifier=? #'unpack* replaceable-unpack*-id)
      #`(#,replacement-unpack*-id $-name e #,depth-stx)]
     [e
      #`(#,generic-unpack*-id #,$-name e #,depth-stx)])))

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
  (with-syntax ([((id id-ref id-statinfo ...) ...) idrs]
                [(((sid ...) sid-ref sid-statinfo ...) ...) sidrs])
    (binding-form
     #'syntax-infoer
     #`(#,(string-append "'" (shrubbery-syntax->string e) "'")
        #,pattern
        #,repack-id
        (id ...)
        (id-ref ...)
        ((id-statinfo ...) ...)
        ((sid ...) ...)
        (sid-ref ...)
        ((sid-statinfo ...) ...)))))

(define-syntax #%quotes
  (expression-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx)
     (define-values (form tail)
       (quoted-shape-dispatch stx
                              in-expression-space
                              convert-template
                              #f
                              convert-template
                              (lambda (e) #`(quote-syntax #,e))))
     (define q-stx (syntax-parse stx [(_ q . _) #'q]))
     (values (relocate-wrapped q-stx form)
             tail))))

(define-binding-syntax #%quotes
  (binding-prefix-operator
   #f
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
                                            ()
                                            ())))))))

(define-repetition-syntax #%quotes
  (repetition-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx)
     (quoted-shape-dispatch stx
                            in-expression-space
                            convert-repetition-template
                            #f
                            convert-repetition-template
                            (lambda (e) (make-repetition-info (list stx)
                                                              null
                                                              #`(quote-syntax #,e)
                                                              (get-syntax-static-infos)
                                                              0))))))

(define-syntax (syntax-infoer stx)
  (syntax-parse stx
    [(_ static-infos (annotation-str pattern repack
                                     (id ...) id-refs (id-statinfos ...)
                                     (sids ...) sid-refs (sid-statinfos ...)))
     (define (make-sequencers depth) (for/list ([i (in-range (syntax-e depth))]) #'in-list))
     (with-syntax ([(id-sequencers ...) (for/list ([id-ref (in-list (syntax->list #'id-refs))])
                                          (syntax-parse id-ref
                                            #:datum-literals (maybe-syntax-wrap)
                                            [(maybe-syntax-wrap (pack _ depth) . _) (make-sequencers #'depth)]
                                            [(pack _ depth) (make-sequencers #'depth)]))]
                   [((sid sid-sequencers) ...)
                    (for/list ([sids (in-list (syntax->list #'(sids ...)))]
                               [sid-ref (in-list (syntax->list #'sid-refs))])
                      (define id+depth
                        (extract-pattern-variable-bind-id-and-depth sids sid-ref))
                      (list (car id+depth) (make-sequencers (cadr id+depth))))]
                   [tmp-ids (generate-temporaries #'(id ...))])
       (binding-info #'annotation-str
                     #'syntax
                     #'()
                     #'((id ((#:repet id-sequencers)) . id-statinfos)
                        ...
                        (sid ((#:repet sid-sequencers)) . sid-statinfos)
                        ...)
                     #'empty-oncer
                     #'syntax-matcher
                     #'tmp-ids
                     #'syntax-committer
                     #'syntax-binder
                     #'(pattern repack tmp-ids (id ...) id-refs (sids ...) sid-refs)))]))

(define-syntax (syntax-matcher stx)
  (syntax-parse stx
    [(_ arg-id (pattern repack (tmp-id ...) (id ...) (id-ref ...) (sid ...) (sid-ref ...))
        IF success fail)
     #'(IF (syntax*? arg-id)
           (begin
             (define-values (match? tmp-id ...)
               (syntax-parse (repack arg-id)
                 #:disable-colon-notation
                 [pattern
                  ;; use `let*` to allow a syntax-class synatx-wrap construction
                  ;; to refer to fields, which are placed earlier
                  (let* ([id id-ref]
                         ...)
                    (values #t id ...))]
                 [_ (values #f 'id ...)]))
             (IF match?
                 success
                 fail))
           fail)]))

(define-syntax (syntax-committer stx)
  (syntax-parse stx
    [(_ arg-id (evidence/tmp-id ...) (pattern repack (tmp-id ...) (id ...) (id-ref ...) (sid ...) (sid-ref ...)))
     #'(begin
         (define id evidence/tmp-id)
         ...)]))

(define-syntax (syntax-binder stx)
  (syntax-parse stx
    [(_ arg-id (evidence/tmp-id ...) (pattern repack (tmp-id ...) (id ...) (id-ref ...) ((sid ...) ...) (sid-ref ...)))
     #'(begin
         (define-syntaxes (sid ...) sid-ref)
         ...)]))

(define-for-syntax (handle-syntax-parse-dispatch who
                                                 val-ids b-parsedss rhss
                                                 make-not-stx-expr
                                                 default-k)
  (cond
    [(and (not (null? b-parsedss))
          (for/and ([parseds (in-list b-parsedss)])
            (unless (null? (cdr parseds))
              (error "handle-syntax-parse-dispatch: must only apply to single-value pattern"))
            (syntax-parse (car parseds)
              [b::binding-form
               (free-identifier=? #'b.infoer-id #'syntax-infoer)])))
     ;; since all bindings are syntax patterns, generate a `syntax-parse` form
     ;; without an "else" case, so it synthesize a message when matching fails
     (define repack-multi?
       (for/or ([parseds (in-list b-parsedss)])
         (syntax-parse (car parseds)
           [b::binding-form
            (syntax-parse #'b.data
              [(annotation-str pattern repack . _)
               (free-identifier=? #'repack #'repack-as-multi)])])))
     #`(if (syntax*? #,(car val-ids))
           (syntax-parse (#,(if repack-multi? #'repack-as-multi #'repack-as-term) #,(car val-ids))
             #:disable-colon-notation
             #:context '#,who
             #,@(for/list ([parseds (in-list b-parsedss)]
                           [rhs (in-list rhss)])
                  (syntax-parse (car parseds)
                    [b::binding-form
                     #:with b-impl::binding-impl #'(b.infoer-id () b.data)
                     #:with b-info::binding-info #'b-impl.info
                     #:with (pattern repack tmp-ids (id ...) (id-ref ...) ((sid ...) ...) (sid-ref ...)) #'b-info.data
                     #`[#,(if (and repack-multi?
                                   (free-identifier=? #'repack #'repack-as-term))
                              #'((~datum multi) ((~datum group) pattern))
                              #'pattern)
                        (define id id-ref)
                        ...
                        (define-syntaxes (sid ...) sid-ref)
                        ...
                        (rhombus-body-expression #,rhs)]])))
           #,(make-not-stx-expr))]
    [else
     (default-k)]))
