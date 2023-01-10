#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     "operator-parse.rkt"
                     "srcloc.rkt")
         syntax/parse/pre
         "parse.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "pack.rkt"
         "empty-group.rkt"
         "syntax-class.rkt"
         "operator-parse.rkt"
         (submod "syntax-class.rkt" for-quasiquote)
         (only-in "underscore.rkt"
                  [_ rhombus-_])
         "dollar.rkt"
         "repetition.rkt"
         "op-literal.rkt"
         "static-info.rkt"
         (for-template "parens.rkt")
         (submod "syntax-object.rkt" for-quasiquote)
         (only-in "annotation.rkt"
                  ::))

(provide #%quotes
         syntax_term
         $
         $$)

(module+ convert
  (begin-for-syntax
    (provide convert-pattern
             convert-template
             (struct-out pattern-variable))))

(begin-for-syntax
  (define-syntax-class (list-repetition in-space)
    (pattern ((~datum op) (~var name (:... in-space))))
    (pattern ((~datum group) ((~datum op) (~var name (:... in-space)))))
    (pattern ((~datum block) ((~datum group) ((~datum op) (~var name (:... in-space)))))))
  (define-splicing-syntax-class (tail-repetition in-space)
    #:datum-literals (op)
    (pattern (~seq (op (~var _ (:$ in-space))) e:identifier (op (~var name (:... in-space))))))
  (define-splicing-syntax-class (block-tail-repetition in-space)
    #:datum-literals (op)
    (pattern (~seq ((~datum group) (op (~var _ (:$ in-space))) e:identifier)
                   ((~datum group) (op (~var name (:... in-space)))))))

  (struct pattern-variable (id val-id depth unpack*-id)))

(define-for-syntax (make-pattern-variable-syntax name-id temp-id unpack* depth splice? attributes)
  (define (lookup-attribute stx var-id attr-id want-repet?)
    (define attr (hash-ref attributes (syntax->datum attr-id) #f))
    (unless (and attr (eq? want-repet? (not (eqv? 0 (+ depth
                                                       (if splice? -1 0)
                                                       (syntax-class-attribute-depth attr))))))
      (raise-syntax-error #f
                          (format
                           (string-append (if attr
                                              (if want-repet?
                                                  "attribute is not a repetition\n"
                                                  "attribute is a repetition\n")
                                              "attribute not found\n")
                                          "  pattern: ~a\n"
                                          "  attribute: ~a")
                           (syntax-e var-id)
                           (syntax-e attr-id))
                          stx))
    attr)
  (define expr-handler
    (lambda (stx fail)
      (syntax-parse stx
        #:datum-literals (op |.|)
        [(var-id (op |.|) attr-id . tail)
         (define attr (lookup-attribute stx #'var-id #'attr-id #f))
         (values (wrap-static-info* (syntax-class-attribute-id attr)
                                    syntax-static-infos)
                 #'tail)]
        [_ (fail)])))
  (define id-handler
    (lambda (stx)
      (syntax-parse stx
        [(_ . tail) (values (wrap-static-info* temp-id syntax-static-infos) #'tail)])))
  (cond
    [(eq? depth 0) (if (eq? 0 (hash-count attributes))
                       (expression-transformer
                        name-id
                        id-handler)
                       (expression-transformer
                        name-id
                        (lambda (stx)
                          (expr-handler stx
                                        (lambda ()
                                          (id-handler stx))))))]
    [else (make-repetition
           name-id
           #`(#,unpack* #'$ #,temp-id #,depth)
           syntax-static-infos
           #:depth depth
           #:repet-handler (lambda (stx next)
                             (syntax-parse stx
                               #:datum-literals (op |.|)
                               [(var-id (~and dot-op (op |.|)) attr-id . tail)
                                (define attr (lookup-attribute stx #'var-id #'attr-id #t))
                                (values (make-repetition-info #'(var-id dot-op attr-id)
                                                              (string->symbol
                                                               (format "~a.~a" (syntax-e #'var-id) (syntax-e #'attr-id)))
                                                              (syntax-class-attribute-id attr)
                                                              (+ (syntax-class-attribute-depth attr) depth (if splice? -1 0))
                                                              #'0
                                                              syntax-static-infos
                                                              #f)
                                        #'tail)]
                               [_ (next)]))
           #:expr-handler expr-handler)]))

(define-for-syntax (convert-syntax e make-datum make-literal
                                   handle-escape handle-group-escape handle-multi-escape
                                   adjust-escape-siblings deepen-escape deepen-syntax-escape
                                   handle-tail-escape handle-block-tail-escape
                                   handle-maybe-empty-sole-group
                                   handle-maybe-empty-alts handle-maybe-empty-group
                                   #:in-space in-space
                                   #:tail-any-escape? [tail-any-escape? #f]
                                   #:as-tail? [as-tail? #f]
                                   #:splice? [splice? #f]
                                   #:splice-pattern [splice-pattern #f]
                                   #:allow-fltten? [allow-flatten? #f]
                                   #:flatten-escape [flatten-escape #f])
  (let convert ([e e] [empty-ok? splice?] [depth 0] [as-tail? as-tail?] [splice? splice?])
    (syntax-parse e
      #:datum-literals (parens brackets braces block quotes multi group alts)
      [(group
        (op (~var $-id (:$ in-space))) esc)
       #:when (and (zero? depth) (not as-tail?) (not splice?))
       ;; Special case: a group whose content is an escape; the escape
       ;; defaults to "group" mode instead of "term" mode
       #:do [(define-values (p new-idrs new-sidrs new-vars) (handle-group-escape #'$-id #'esc e))]
       #:when p
       (values p new-idrs new-sidrs new-vars #f)]
      [((~and tag (~or parens brackets braces quotes multi block))
        (group (op (~var $-id (:$ in-space))) esc))
       #:when (and (zero? depth) (not as-tail?))
       ;; Analogous special case, but for blocks (maybe within an `alts`), etc.
       #:do [(define-values (p new-idrs new-sidrs new-vars) (handle-multi-escape #'$-id #'esc e splice?))]
       #:when p
       (values p new-idrs new-sidrs new-vars #f)]
      [((~and tag (~or parens brackets braces quotes multi block))
        (~and g (group . _)))
       #:when (not splice?)
       ;; Special case: for a single group with (), [], {}, '', or block, if the group
       ;; can be empty, allow a match/construction with zero groups
       (define-values (p new-idrs new-sidrs new-vars can-be-empty?) (convert #'g #t depth as-tail? #f))
       (if can-be-empty?
           (handle-maybe-empty-sole-group #'tag p new-idrs new-sidrs new-vars)
           (values (quasisyntax/loc e (#,(make-datum #'tag) #,p))
                   new-idrs
                   new-sidrs
                   new-vars
                   #f))]
      [((~and tag (~or parens brackets braces quotes multi block alts group))
        g ...)
       ;; Note: this is where `depth` would be incremented, when `tag` is `quotes`, if we wanted that
       (let loop ([gs #'(g ...)] [pend-idrs #f] [pend-sidrs #f] [pend-vars #f]
                                 [idrs '()]  ; list of #`[#,id #,rhs] for definitions
                                 [sidrs '()] ; list of #`[#,id #,rhs] for syntax definitions
                                 [vars '()]  ; list of `[,id . ,depth] for visible subset of `idrs` and `sidrs`
                                 [ps '()] [can-be-empty? #t] [pend-is-rep? #f] [tail #f] [depth depth])
         (define really-can-be-empty? (and can-be-empty? (or pend-is-rep? (not pend-idrs))))
         (define (simple gs a-depth)
           (syntax-parse gs
             [(g . gs)
              (define-values (p new-ids new-sidrs new-vars nested-can-be-empty?) (convert #'g #f a-depth #f #f))
              (loop #'gs new-ids new-sidrs new-vars
                    (append (or pend-idrs '()) idrs)
                    (append (or pend-sidrs '()) sidrs)
                    (append (or pend-vars '()) vars)
                    (cons p ps) really-can-be-empty? #f #f depth)]))
         (define (simple2 gs a-depth)
           (syntax-parse gs
             [(g0 g1 . gs)
              (define-values (p0 new-ids0 new-sids0 new-vars0 nested-can-be-empty?0) (convert #'g0 #f a-depth #f #f))
              (define-values (p1 new-ids1 new-sids1 new-vars1 nested-can-be-empty?1) (convert #'g1 #f a-depth #f #f))
              (loop #'gs (append new-ids0 new-ids1) (append new-sids0 new-sids1) (append new-vars0 new-vars1)
                    (append (or pend-idrs '()) idrs)
                    (append (or pend-sidrs '()) sidrs)
                    (append (or pend-vars '()) vars)
                    (list* p1 p0 ps) really-can-be-empty? #f #f depth)]))
         (syntax-parse gs
           [()
            (let ([ps (let ([ps (reverse ps)])
                        (if tail
                            (append ps tail)
                            ps))]
                  [idrs (append (or pend-idrs '()) idrs)]
                  [sidrs (append (or pend-sidrs '()) sidrs)]
                  [vars (append (or pend-vars '()) vars)]
                  [can-be-empty? really-can-be-empty?])
              (cond
                [(and can-be-empty? (eq? (syntax-e #'tag) 'alts))
                 (handle-maybe-empty-alts #'tag ps idrs sidrs vars)]
                [(and can-be-empty? (eq? (syntax-e #'tag) 'group) (not empty-ok?))
                 (handle-maybe-empty-group #'tag ps idrs sidrs vars)]
                [else
                 (values
                  (if splice?
                      (if splice-pattern
                          (splice-pattern ps)
                          (quasisyntax/loc e (~seq . #,ps)))
                      (quasisyntax/loc e (#,(make-datum #'tag) . #,ps)))
                  idrs
                  sidrs
                  vars
                  can-be-empty?)]))]
           [((~var op (tail-repetition in-space)))
            #:when (and (zero? depth)
                        (or tail-any-escape?
                            (identifier? #'op.e))
                        (not splice?))
            (define-values (id new-idrs new-sidrs new-vars) (handle-tail-escape #'op.name #'op.e e))
            (loop #'() #f #f #f
                  (append new-idrs (or pend-idrs '()) idrs)
                  (append new-sidrs (or pend-sidrs '()) sidrs)
                  (append new-vars (or pend-vars '()) vars)
                  ps really-can-be-empty? #f id depth)]
           [((~var op (block-tail-repetition in-space)))
            #:when (and (zero? depth)
                        (or tail-any-escape?
                            (identifier? #'op.e)))
            (define-values (id new-idrs new-sidrs new-vars) (handle-block-tail-escape #'op.name #'op.e e))
            (loop #'() #f #f #f
                  (append new-idrs (or pend-idrs '()) idrs)
                  (append new-sidrs (or pend-sidrs '()) sidrs)
                  (append new-vars (or pend-vars '()) vars)
                  ps really-can-be-empty? #f id depth)]
           [((~var op (list-repetition in-space)) . gs)
            #:when (zero? depth)
            (unless pend-idrs
              (raise-syntax-error #f
                                  "misplaced repetition"
                                  #'op.name))
            (define adj-pend-idrs (adjust-escape-siblings pend-idrs))
            (define new-pend-idrs (for/list ([idr (in-list adj-pend-idrs)])
                                    (deepen-escape idr)))
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
                      (cons (quote-syntax ...) ps) can-be-empty? #t #f depth)
                (loop #'gs #f #f #f
                      (append new-pend-idrs idrs)
                      (append new-pend-sidrs sidrs)
                      (append new-pend-vars vars)
                      (cons (quote-syntax ...) ps) can-be-empty? #f #f depth))]
           [(((~datum op) (~and $-id (~or (~var _ (:$ in-space)) (~literal $$)))) esc . n-gs)
            #:when (or flatten-escape
                       (not (free-identifier=? #'$-id #'$$)))
            (cond
              [(zero? depth)
               (define flatten? (and flatten-escape
                                     (free-identifier=? #'$-id #'$$)))
               (define-values (pat new-idrs new-sidrs new-vars) (handle-escape #'$-id #'esc e))
               (define adj-new-idrs
                 (cond
                   [flatten? (map flatten-escape new-idrs)]
                   [else new-idrs]))
               (loop #'n-gs adj-new-idrs new-sidrs new-vars
                     (append (or pend-idrs '()) idrs)
                     (append (or pend-sidrs '()) sidrs)
                     (append (or pend-vars '()) vars)
                     (if flatten? (list* (quote-syntax ...) pat ps) (cons pat ps))
                     really-can-be-empty? #f #f depth)]
              [else
               (simple2 gs (sub1 depth))])]
           [(g . _)
            (simple gs depth)]))]
      [((~and tag op) op-name)
       (values (quasisyntax/loc e (#,(make-datum #'tag) #,(make-literal #'op-name))) null null null #f)]
      [id:identifier
       (values (make-literal #'id) null null null #f)]
      [_
       (values e null null null #f)])))

(define-for-syntax (convert-pattern e
                                    #:as-tail? [as-tail? #f]
                                    #:splice? [splice? #f]
                                    #:splice-pattern [splice-pattern #f])
  (define (make-datum d)
    (case (syntax-e d)
      [(parens) #'_::parens]
      [(block) #'_::block]
      [(alts) #'_::alts]
      [(braces) #'_::braces]
      [(brackets) #'_::brackets]
      [else #`(~datum #,d)]))
  (define (handle-escape $-id e in-e pack* unpack* context-syntax-class kind)
    (syntax-parse e
      #:datum-literals (parens op group quotes)
      [(~var _ (:_ in-binding-space))
       ;; match anything
       (values #'_ null null null)]
      [_:identifier
       #:with (tag . _) in-e
       (let* ([temps (generate-temporaries (list e e))]
              [temp1 (car temps)]
              [temp2 (cadr temps)])
         (values temp1
                 (list #`[#,temp2 (#,pack* (syntax #,temp1) 0)])
                 (list #`[#,e (make-pattern-variable-syntax (quote-syntax e)
                                                            (quote-syntax #,temp2)
                                                            (quote-syntax #,unpack*)
                                                            0
                                                            #f
                                                            (hasheq))])
                 (list (pattern-variable e temp2 0 unpack*))))]
      [(parens (group id:identifier (op colons) stx-class:identifier))
       #:when (free-identifier=? #':: (in-binding-space #'colons))
       (define rsc (syntax-local-value (in-syntax-class-space #'stx-class) (lambda () #f)))
       (define (compat pack* unpack*)
         (define sc (rhombus-syntax-class-class rsc))
         (define temp0-id (car (generate-temporaries (list #'id))))
         (define temp-id (car (generate-temporaries (list #'id))))
         (define-values (attribute-bindings attribute-mappings)
           (for/lists (bindings mappings)
                      ([name+depth (in-list (rhombus-syntax-class-attributes rsc))]
                       [temp-attr (in-list (generate-temporaries (map car (rhombus-syntax-class-attributes rsc))))])
             (define name (car name+depth))
             (define depth (cdr name+depth))
             (define id-with-attr
               (datum->syntax temp0-id (string->symbol (format "~a.~a" (syntax-e temp0-id) name))))
             (values #`[#,temp-attr (pack-term*
                                     (syntax #,(let loop ([t id-with-attr] [depth depth])
                                                 (if (zero? depth)
                                                     t
                                                     (loop #`(#,t #,(quote-syntax ...)) (sub1 depth)))))
                                     #,depth)]
                     (cons name (cons temp-attr depth)))))
         (define pack-depth (if (rhombus-syntax-class-splicing? rsc) 1 0))
         (values (if sc
                     #`(~var #,temp0-id #,sc)
                     temp0-id)
                 (cons #`[#,temp-id (#,pack* (syntax #,temp0-id) #,pack-depth)] attribute-bindings)
                 (list #`[id (make-pattern-variable-syntax
                              (quote-syntax id)
                              (quote-syntax #,temp-id)
                              (quote-syntax #,unpack*)
                              #,pack-depth
                              #,(rhombus-syntax-class-splicing? rsc)
                              (hasheq #,@(apply append (for/list ([b (in-list attribute-mappings)])
                                                         (list #`(quote #,(car b))
                                                               #`(syntax-class-attribute (quote-syntax #,(cadr b))
                                                                                         #,(cddr b)))))))])
                 (list (pattern-variable #'id temp-id pack-depth unpack*))))
       (define (incompat)
         (raise-syntax-error #f
                             "syntax class incompatible with this context"
                             in-e
                             #'stx-class))
       (cond
         [(not (rhombus-syntax-class? rsc))
          (raise-syntax-error #f
                              "not bound as a syntax class"
                              in-e
                              #'stx-class)]
         [(eq? (rhombus-syntax-class-kind rsc) 'term)
          (cond
            [(not (eq? kind 'term))
             (values #f #f #f #f)]
            [else (compat pack* unpack*)])]
         [(eq? (rhombus-syntax-class-kind rsc) 'group)
          (cond
            [(eq? kind 'term) (incompat)]
            [(not (eq? kind 'group)) (values #f #f #f #f)]
            [else (compat pack* unpack*)])]
         [(eq? (rhombus-syntax-class-kind rsc) 'multi)
          (cond
            [(eq? kind 'multi) (compat pack* unpack*)]
            [else (incompat)])]
         [(eq? (rhombus-syntax-class-kind rsc) 'block)
          (cond
            [(and (eq? kind 'multi) (syntax-parse in-e
                                      [(head . _) (memq (syntax-e #'head) '(block alts))]))
             (compat #'pack-block* #'unpack-multi-as-term*)]
            [else (incompat)])]
         [else
          (error "unrecognized kind" kind)])]
      [(quotes (group (op name))) (if (eq? kind 'term)
                                      (values #'((~datum op) (~datum name)) null null null)
                                      (values #f #f #f #f))]
      [(parens (group (quotes (group (op name))))) (if (eq? kind 'term)
                                                       (values #'((~datum op) (~datum name)) null null null)
                                                       (values #f #f #f #f))]
      [_ (raise-syntax-error #f "invalid pattern escape" e)]))
  (define (handle-escape/match-head $-id e in-e pack* unpack* context-syntax-class kind splice?)
    (define-values (p idrs sidrs vars) (handle-escape $-id e in-e pack* unpack* context-syntax-class kind))
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
                    #`(~literal #,d))
                  ;; handle-escape:
                  (lambda ($-id e in-e)
                    (handle-escape $-id e in-e #'pack-term* #'unpack-term* #'Term 'term))
                  ;; handle-group-escape:
                  (lambda ($-id e in-e)
                    (handle-escape/match-head $-id e in-e #'pack-group* #'unpack-group* #'Group 'group #f))
                  ;; handle-multi-escape:
                  (lambda ($-id e in-e splice?)
                    (handle-escape/match-head $-id e in-e #'pack-tagged-multi* #'unpack-multi-as-term* #'Multi 'multi splice?))
                  ;; adjust-escape-siblings
                  (lambda (idrs)
                    idrs)
                  ;; deepen-escape
                  (lambda (idr)
                    (syntax-parse idr
                      [(id (pack (_ stx) depth))
                       #`(id (pack (syntax (stx (... ...))) #,(add1 (syntax-e #'depth))))]))
                  ;; deepen-syntax-escape
                  (lambda (sidr)
                    (syntax-parse sidr
                      [(id (make-pattern-variable-syntax self-id temp-id unpack* depth splice? ht))
                       #`(id (make-pattern-variable-syntax self-id temp-id unpack* #,(add1 (syntax-e #'depth)) splice? ht))]))
                  ;; handle-tail-escape:
                  (lambda (name e in-e)
                    (syntax-parse e
                      [(~var _ (:_ in-binding-space))
                       (values #'_ null null null)]
                      [_
                       (let ([temp0-id (car (generate-temporaries (list e)))]
                             [temp-id (car (generate-temporaries (list e)))])
                         (values temp0-id
                                 (list #`[#,temp-id (pack-tail* (syntax #,temp0-id) 0)])
                                 (list #`[#,e (make-pattern-variable-syntax (quote-syntax #,e)
                                                                            (quote-syntax #,temp-id)
                                                                            (quote-syntax unpack-tail-list*)
                                                                            1
                                                                            #f
                                                                            (hasheq))])
                                 (list (pattern-variable e temp-id 1 (quote-syntax unpack-tail-list*)))))]))
                  ;; handle-block-tail-escape:
                  (lambda (name e in-e)
                    (let ([temp0-id (car (generate-temporaries (list e)))]
                          [temp-id (car (generate-temporaries (list e)))])
                      (values temp0-id
                              (list #`[#,temp-id (pack-multi-tail* (syntax #,temp0-id) 0)])
                              (list #`[#,e (make-pattern-variable-syntax (quote-syntax #,e)
                                                                         (quote-syntax #,temp-id)
                                                                         (quote-syntax unpack-multi-tail-list*)
                                                                         1
                                                                         #f
                                                                         (hasheq))])
                              (list (pattern-variable e temp-id 1 (quote-syntax unpack-multi-tail-list*))))))
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
                  ;; handle-maybe-empty-group
                  (lambda (tag ps idrs sidrs vars)
                    ;; the `(tag . ps)` could match `(group)`, but it just never will,
                    ;; because that won't be an input
                    (values #`((~datum #,tag) . #,ps) idrs sidrs vars #t))))

(define-for-syntax (convert-template e
                                     #:check-escape [check-escape (lambda (e) (void))]
                                     #:rhombus-expression [rhombus-expression #'rhombus-expression]
                                     #:repetition? [repetition? #f])
  (define-values (template idrs sidrs vars can-be-empty?)
    (convert-syntax e
                    #:in-space in-expression-space
                    #:tail-any-escape? #t
                    #:allow-fltten? #t
                    ;; make-datum
                    (lambda (d) d)
                    ;; make-literal
                    (lambda (d) d)
                    ;; handle-escape:
                    (lambda ($-id e in-e)
                      (check-escape e)
                      (define id (car (generate-temporaries (list e))))
                      (values id (list #`[#,id (pending-unpack #,e unpack-term* (quote-syntax #,$-id))]) null null))
                    ;; handle-group-escape:
                    (lambda ($-id e in-e)
                      (check-escape e)
                      (define id (car (generate-temporaries (list e))))
                      (values id (list #`[#,id (pending-unpack #,e unpack-group* (quote-syntax #,$-id))]) null null))
                    ;; handle-multi-escape:
                    (lambda ($-id e in-e splice?)
                      (check-escape e)
                      (define id (car (generate-temporaries (list e))))
                      (with-syntax ([(tag . _) in-e])
                        (values #`(tag . #,id) (list #`[#,id (pending-unpack #,e unpack-multi* (quote-syntax #,$-id))]) null null)))
                    ;; adjust-escape-siblings
                    (lambda (idrs)
                      ;; adapt to allow repetitions at different depths where
                      ;; shallower reptitions are copied to match deeper ones
                      (adjust-template-sibling-depths idrs))
                    ;; deepen-escape
                    (lambda (idr)
                      (deepen-template-escape idr))
                    #:flatten-escape
                    (lambda (idr)
                      (flatten-template-escape idr))
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
                              (cons #`[#,id (convert-empty-alts 0 (#,(quote-syntax quasisyntax) (#,tag . #,ts)))]
                                    idrs)
                              sidrs
                              vars
                              #f))
                    ;; handle-maybe-empty-group
                    (lambda (tag ts idrs sidrs vars)
                      ;; if `(tag . ts)` generates `(group)`, then error
                      (define id (car (generate-temporaries '(group))))
                      (values id
                              (cons #`[#,id (error-empty-group 0 (#,(quote-syntax quasisyntax) (#,tag . #,ts)))]
                                    idrs)
                              sidrs
                              vars
                              #f))))
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
                                       syntax-static-infos
                                       #f)]
    [else (wrap-static-info* template-e
                             syntax-static-infos)]))

(define-for-syntax (convert-repetition-template e)
  (convert-template e #:repetition? #t))

(define-for-syntax (deepen-template-escape idr)
  (syntax-parse idr
    #:literals (unpacking)
    [(id-pat (unpacking depth 0 . u))
     #`[(id-pat (... ...)) (unpacking #,(add1 (syntax-e #'depth)) 0 . u)]]
    [(id-pat (unpacking depth k . u))
     #`[id-pat (unpacking depth #,(sub1 (syntax-e #'k)) . u)]]
    [(id-pat (converter depth (qs t) . args))
     #`[(id-pat (... ...)) (converter #,(add1 (syntax-e #'depth)) (qs (t (... ...)))) . args]]))

(define-for-syntax (flatten-template-escape idr)
  (syntax-parse idr
    #:literals (pending-unpack unpack-term*)
    [[id-pat (pending-unpack e unpack-term* . u)]
     #`[(id-pat (... ...)) (pending-unpack e unpack-term-list* . u)]]))

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
                  (map deepen-template-escape
                       (adjust-template-sibling-depths idrs))
                  #`(#,template (... ...)))])))

;; if we get here, it means that an escape was not under `...`
(define-syntax (pending-unpack stx)
  (syntax-parse stx
    [(_ e unpack* $-name) #'(unpack* $-name (rhombus-expression (group e)) 0)]))

;; if we get here, it means that an escape was under some number of `...`
(define-syntax (unpacking stx)
  (syntax-parse stx
    [(_ depth k rep-info::repetition-info unpack* $-name)
     (define base-e (repetition-as-list #'rep-info (syntax-e #'depth)))
     (define unpack*-id #'unpack*)
     (cond
       [(free-identifier=? unpack*-id #'unpack-tail*)
        (get-tail-repetition #'$-name base-e (sub1 (syntax-e #'depth))
                             #'unpack-tail-list* #'unpack-tail* #'unpack-list-tail*)]
       [(free-identifier=? unpack*-id #'unpack-multi-tail*)
        (get-tail-repetition #'$-name base-e (sub1 (syntax-e #'depth))
                             #'unpack-multi-tail-list* #'unpack-multi-tail* #'unpack-multi-list-tail*)]
       [else
        ;; strip away redundant unpack as an optimization
        (define opt-e
          (let loop ([e base-e])
            (syntax-parse e
              #:literals (begin quote-syntax)
              [(begin (quote-syntax . _) e) (loop #'e)]
              [(unpack*:id _ e d)
               #:when (free-identifier=? #'unpack* unpack*-id)
               #'e]
              [_ e])))
        #`(unpack* $-name #,opt-e depth)])]))

(define-for-syntax (get-tail-repetition $-name base-e depth-stx
                                        replaceable-unpack*-id replacement-unpack*-id generic-unpack*-id)
  ;; replace redundant unpack with alternative; this is not just an
  ;; optimization, but a change to the time complexity of using the
  ;; tail in a template by avoiding conversion to a list and back
  (let loop ([e base-e])
    (syntax-parse e
      #:literals (begin quote-syntax)
      [(begin (quote-syntax . _) e) (loop #'e)]
      [(unpack* $-name e d)
       #:when (free-identifier=? #'unpack* replaceable-unpack*-id)
       #`(#,replacement-unpack*-id $-name e #,depth-stx)]
      [_
       #`(#,generic-unpack*-id #,$-name #,e #,depth-stx)])))

(define-for-syntax (call-with-quoted-expression stx in-space single-k multi-k literal-k)
  (syntax-parse stx
    #:datum-literals (quotes group op)
    [(_ (quotes (group (~and special (op (~or (~var _ (:... in-space)) (~var _ (:$ in-space))))))) . tail)
     (values (literal-k #'special)
             #'tail)]
    [(_ (quotes (group t)) . tail)
     (values (single-k #'t)
             #'tail)]
    [(_ ((~and tag quotes) . args) . tail)
     (values (multi-k (datum->syntax #f (cons (syntax-property (datum->syntax #f 'multi) 'raw "")
                                              #'args)))
             #'tail)]))

(define-for-syntax ((convert-pattern/generate-match repack-id) e)
  (define-values (pattern idrs sidrs vars can-be-empty?) (convert-pattern e))
  (with-syntax ([((id id-ref) ...) idrs]
                [((sid sid-ref) ...) sidrs])
    (with-syntax ([(tmp-id ...) (generate-temporaries #'(id ...))])
      (binding-form
       #'syntax-infoer
       #`(#,(string-append "'" (shrubbery-syntax->string e) "'")
          #,pattern
          #,repack-id
          (tmp-id ...)
          (id ...)
          (id-ref ...)
          (sid ...)
          (sid-ref ...))))))

(define-syntax #%quotes
  (make-expression+binding+repetition-prefix-operator
   #'#%quotes
   '((default . stronger))
   'macro
   (lambda (stx)
     (call-with-quoted-expression stx
                                  in-expression-space
                                  convert-template
                                  convert-template
                                  (lambda (e) #`(quote-syntax #,e))))
   (lambda (stx)
     (call-with-quoted-expression stx
                                  in-binding-space
                                  (convert-pattern/generate-match #'repack-as-term)
                                  (convert-pattern/generate-match #'repack-as-multi)
                                  (lambda (e) (binding-form
                                               #'syntax-infoer
                                               #`(#,(string-append "'" (shrubbery-syntax->string e) "'")
                                                  #,(syntax-parse e
                                                      [((~datum op) id) #`((~datum op) (~literal id))])
                                                  repack-as-term
                                                  ()
                                                  ()
                                                  ())))))
   (lambda (stx)
     (call-with-quoted-expression stx
                                  in-expression-space
                                  convert-repetition-template
                                  convert-repetition-template
                                  (lambda (e) (make-repetition-info stx
                                                                    #'template
                                                                    #`(quote-syntax #,e)
                                                                    0
                                                                    0
                                                                    #'()
                                                                    #t))))))

(define-syntax syntax_term
  (make-expression+binding-prefix-operator
   (quote-syntax syntax_term)
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group)
       [(_ (parens (group term)) . tail)
        (values (convert-template #'term) #'tail)]))
   ;; pattern
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group)
       [(_ (parens (group term)) . tail)
        (values (convert-pattern/generate-match #'term) #'tail)]))))

(define-syntax (syntax-infoer stx)
  (syntax-parse stx
    [(_ static-infos (annotation-str pattern repack tmp-ids (id ...) id-refs (sid ...) sid-refs))
     (with-syntax ([(id-depth ...) (for/list ([id-ref (in-list (syntax->list #'id-refs))])
                                     (syntax-parse id-ref
                                       [(pack _ depth) #'depth]))]
                   [(sid-depth ...) (for/list ([sid-ref (in-list (syntax->list #'sid-refs))])
                                      (syntax-parse sid-ref
                                        [(make-pattern-variable-syntax _ _ _ depth . _) #'depth]))])
       (binding-info #'annotation-str
                     #'syntax
                     #'()
                     #'((id (id-depth)) ... (sid (sid-depth)) ...)
                     #'syntax-matcher
                     #'syntax-committer
                     #'syntax-binder
                     #'(pattern repack tmp-ids (id ...) id-refs (sid ...) sid-refs)))]))

(define-syntax (syntax-matcher stx)
  (syntax-parse stx
    [(_ arg-id (pattern repack (tmp-id ...) (id ...) (id-ref ...) (sid ...) (sid-ref ...)) IF success fail)
     #'(IF (syntax? arg-id)
           (begin
             (define-values (match? tmp-id ...)
               (syntax-parse (repack arg-id)
                 [pattern (values #t id-ref ...)]
                 [_ (values #f 'id ...)]))
             (IF match?
                 success
                 fail))
           fail)]))

(define-syntax (syntax-committer stx)
  (syntax-parse stx
    [(_ arg-id (pattern repack (tmp-id ...) (id ...) (id-ref ...) (sid ...) (sid-ref ...)))
     #'(begin)]))

(define-syntax (syntax-binder stx)
  (syntax-parse stx
    [(_ arg-id (pattern repack (tmp-id ...) (id ...) (id-ref ...) (sid ...) (sid-ref ...)))
     #'(begin
         (define id tmp-id) ...
         (define-syntax sid sid-ref) ...)]))
