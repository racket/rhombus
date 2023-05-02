#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     "class-parse.rkt"
                     "expose.rkt")
         (submod "annotation.rkt" for-class)
         "entry-point.rkt"
         "parens.rkt")

(provide (for-syntax extract-internal-ids
                     make-expose
                     parse-annotation-options
                     parse-options
                     class-clause-accum
                     class-clause-extract
                     method-shape-extract))

(module+ for-interface
  (provide (for-syntax parse-method-clause
                       extract-rhs)))

(define-for-syntax (extract-internal-ids options
                                         scope-stx base-stx
                                         stxes)
  (define internal-ids (reverse (hash-ref options 'internals '())))
  (define internal-id (and (pair? internal-ids) (car internal-ids)))
  (define extra-internal-ids (if (pair? internal-ids) (cdr internal-ids) '()))
  (define expose (if internal-id
                     (make-expose scope-stx base-stx)
                     (lambda (stx) stx)))
  (values internal-id
          (expose internal-id)
          (map expose extra-internal-ids)))

(define-for-syntax (extract-rhs b)
  (syntax-parse b
    [(_::block g) #'g]
    [else
     (raise-syntax-error #f
                         "expected a single entry point in block body"
                         b)]))

(define-for-syntax (parse-annotation-options orig-stx forms)
  (syntax-parse forms
    #:context orig-stx
    [((_ clause-parsed) ...)
     (let loop ([clauses (syntax->list #'(clause-parsed ...))] [options #hasheq()])
       (cond
         [(null? clauses) options]
         [else
          (define clause (car clauses))
          (define new-options
            (syntax-parse clause
              [(#:extends id)
               (when (hash-has-key? options 'extends)
                 (raise-syntax-error #f "multiple extension clauses" orig-stx #'id))
               (hash-set options 'extends #'id)]
              [(#:internal id)
               (hash-set options 'internals (cons #'id (hash-ref options 'internals '())))]
              [(#:annotation block)
               (when (hash-has-key? options 'annotation-rhs)
                 (raise-syntax-error #f "multiple annotation clauses" orig-stx clause))
               (hash-set options 'annotation-rhs (extract-rhs #'block))]
              [(#:expression block)
               ;; needed to determine whether default `.of` annotation can work
               (hash-set options 'expression-rhs #t)]
              [_ options]))
          (loop (cdr clauses) new-options)]))]))

(define-for-syntax (parse-options orig-stx forms)
  (syntax-parse forms
    #:context orig-stx
    [((_ clause-parsed) ...)
     (define clauses (syntax->list #'(clause-parsed ...)))
     (define (add-implements options extra-key ids-stx)
       (define l (reverse (syntax->list ids-stx)))
       (define new-options
         (hash-set options 'implements (append l (hash-ref options 'implements '()))))
       (if extra-key
           (hash-set new-options extra-key (append l (hash-ref new-options extra-key '())))
           new-options))
     (let loop ([clauses clauses] [options #hasheq()])
       (cond
         [(null? clauses) options]
         [else
          (define clause (car clauses))
          (define new-options
            (syntax-parse clause
              [(#:extends id) ; checked in `parse-annotation-options`
               (hash-set options 'extends #'id)]
              [(#:implements id ...)
               (add-implements options 'public-implements #'(id ...))]
              [(#:private-implements id ...)
               (add-implements options 'private-implements #'(id ...))]
              [(#:internal id)
               (hash-set options 'internals (cons #'id (hash-ref options 'internals '())))]
              [(#:constructor id rhs)
               (when (hash-has-key? options 'constructor-rhs)
                 (raise-syntax-error #f "multiple constructor clauses" orig-stx clause))
               (define rhs-options (hash-set options 'constructor-rhs #'rhs))
               (if (syntax-e #'id)
                   (hash-set rhs-options 'constructor-name #'id)
                   rhs-options)]
              [(#:expression rhs)
               (when (hash-has-key? options 'expression-rhs)
                 (raise-syntax-error #f "multiple expression macro clauses" orig-stx clause))
               (hash-set options 'expression-rhs (extract-rhs #'rhs))]
              [(#:binding block)
               (when (hash-has-key? options 'binding-rhs)
                 (raise-syntax-error #f "multiple binding clauses" orig-stx clause))
               (hash-set options 'binding-rhs (extract-rhs #'block))]
              [(#:annotation block) ; checked in `parse-annotation-options`
               (hash-set options 'annotation-rhs (extract-rhs #'block))]
              [(#:dot name block)
               (hash-set options 'dots (cons (cons #'name (extract-rhs #'block))
                                             (hash-ref options 'dots null)))]
              [(#:nonfinal)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "multiple finality clauses" orig-stx clause))
               (hash-set options 'final? #f)]
              [(#:authentic)
               (when (hash-has-key? options 'authentic?)
                 (raise-syntax-error #f "multiple authenticity clauses" orig-stx clause))
               (hash-set options 'authentic? #t)]
              [(#:opaque)
               (when (hash-has-key? options 'opaque?)
                 (raise-syntax-error #f "multiple opacity clauses" orig-stx clause))
               (hash-set options 'opaque? #t)]
              [(#:field id rhs-id ann-seq blk form-id mode)
               (with-syntax ([(predicate annotation-str static-infos)
                              (syntax-parse #'ann-seq
                                [#f (list #'#f #'#f #'())]
                                [(c::inline-annotation)
                                 (list #'c.predicate #'c.annotation-str #'c.static-infos)])])
                 (hash-set options 'fields (cons (added-field #'id
                                                              #'rhs-id #'blk #'form-id
                                                              #'static-infos
                                                              #'predicate
                                                              #'annotation-str
                                                              (syntax-e #'mode))
                                                 (hash-ref options 'fields null))))]
              [_
               (parse-method-clause orig-stx options clause)]))
          (loop (cdr clauses) new-options)]))]))

(define-for-syntax (parse-method-clause orig-stx options clause)
  (syntax-parse clause
    [((~and tag (~or #:method #:override #:private #:final #:final-override #:private-override
                     #:property #:override-property
                     #:final-property #:final-override-property
                     #:private-property #:private-override-property))
      id rhs maybe-ret)
     (define-values (body replace disposition kind)
       (case (syntax-e #'tag)
         [(#:method) (values 'method 'method 'abstract 'method)]
         [(#:override) (values 'method 'override 'abstract 'method)]
         [(#:private) (values 'method 'method 'private 'method)]
         [(#:private-override) (values 'method 'override 'private 'method)]
         [(#:final) (values 'method 'method 'final 'method)]
         [(#:final-override) (values 'method 'override 'final 'method)]
         [(#:property) (values 'method 'method 'abstract 'property)]
         [(#:override-property) (values 'method 'override 'abstract 'property)]
         [(#:final-property) (values 'method 'method 'final 'property)]
         [(#:final-override-property) (values 'method 'override 'final 'property)]
         [(#:private-property) (values 'method 'method 'private 'property)]
         [(#:private-override-property) (values 'method 'override 'private 'property)]
         [else (error "method kind not handled" #'tag)]))
     (hash-set options 'methods (cons (added-method #'id
                                                    (car (generate-temporaries #'(id)))
                                                    #'rhs
                                                    #'maybe-ret
                                                    (and (or (pair? (syntax-e #'maybe-ret))
                                                             (syntax-e #'e-arity.parsed))
                                                         (car (generate-temporaries #'(id))))
                                                    body
                                                    replace
                                                    disposition
                                                    kind
                                                    (extract-arity #'rhs))
                                      (hash-ref options 'methods null)))]
    [((~and tag (~or #:abstract #:abstract-property #:abstract-override #:abstract-override-property))
      id rhs maybe-ret)
     (define-values (replace kind)
       (case (syntax-e #'tag)
         [(#:abstract) (values 'method 'method)]
         [(#:abstract-property) (values 'method 'property)]
         [(#:abstract-override) (values 'override 'method)]
         [(#:abstract-override-property) (values 'override 'property)]
         [else (error "method kind not handled" #'tag)]))
     (hash-set options 'methods (cons (added-method #'id
                                                    '#:abstract
                                                    #'rhs
                                                    #'maybe-ret
                                                    (and (or (pair? (syntax-e #'maybe-ret))
                                                             (syntax-e #'e-arity.parsed))
                                                         (car (generate-temporaries #'(id))))
                                                    'abstract
                                                    replace
                                                    'abstract
                                                    kind
                                                    (extract-arity #'rhs))
                                      (hash-ref options 'methods null)))]
    [_
     (raise-syntax-error #f "unrecognized clause" orig-stx clause)]))

(define-for-syntax (extract-arity rhs)
  (syntax-parse rhs
    [(_ e-arity::entry-point-arity)
     (and (syntax-e #'e-arity.parsed)
          (let ([a (syntax->datum #'e-arity.parsed)])
            (if (exact-integer? a)
                (* 2 a)
                (cons (* 2 (car a)) (cdr a)))))]
    [_ #f]))

(define-for-syntax (class-clause-accum forms)
  ;; early processing of a clause to accumulate information of `class-data`;
  ;; in principle, keep only things that are useful to report to clause macros,
  ;; but for now we just keep everything
  (for/list ([form (in-list (syntax->list forms))])
    (syntax-parse form
      [(_ (_ e) _) #'e])))

(define-for-syntax (class-clause-extract who accum key)
  (define (method id rhs vis)
    (case key
      [(method_names) (list id)]
      [(method_arities) (list (extract-arity rhs))]
      [(method_visibilities) (list vis)]
      [else null]))
  (define (property id rhs vis)
    (case key
      [(property_names) (list id)]
      [(property_arities) (list (extract-arity rhs))]
      [(property_visibilities) (list vis)]
      [else null]))
  (define (extract-arity e)
    (syntax-parse e
      [(_ e-arity::entry-point-arity)
       (syntax->datum #'e-arity.parsed)]
      [_ #f]))
  (for/list ([a (in-list (reverse (syntax->list accum)))]
             #:do [(define v
                     (syntax-parse a
                       [(#:extends id) (if (eq? key 'extends)
                                           (list #'id)
                                           null)]
                       [(#:implements id ...) (case key
                                                [(implements)
                                                 (syntax->list #'(id ...))]
                                                [(implements_visibilities)
                                                 '(public)]
                                                [else null])]
                       [(#:private-implements id ...) (case key
                                                        [(implements)
                                                         (syntax->list #'(id ...))]
                                                        [(implements_visibilities)
                                                         '(private)]
                                                        [else null])]
                       [(#:field id rhs-id ann-seq blk form-id mode)
                        (case key
                          [(field-names) (list #'id)]
                          [(field-visibilities) (list #'mode)]
                          [else null])]
                       [(#:internal id) (case key
                                          [(internal_names) (list #'id)]
                                          [else null])]
                       [(#:method id rhs . _) (method #'id #'rhs 'public)]
                       [(#:override id rhs . _) (method #'id #'rhs 'public)]
                       [(#:private id rhs . _) (method #'id #'rhs 'private)]
                       [(#:private-override id rhs . _) (method #'id #'rhs 'private)]
                       [(#:final id rhs . _) (method #'id #'rhs 'public)]
                       [(#:final-override id rhs . _) (method #'id #'rhs 'public)]
                       [(#:property id rhs . _) (property #'id #'rhs 'public)]
                       [(#:override-property id rhs . _) (property #'id #'rhs 'public)]
                       [(#:final-property id rhs . _) (property #'id #'rhs 'public)]
                       [(#:final-overrode-property id rhs . _) (property #'id #'rhs 'public)]
                       [(#:private-property id rhs . _) (property #'id #'rhs 'private)]
                       [(#:private-override-property id rhs . _) (property #'id #'rhs 'private)]
                       [(#:private-override-property id rhs . _) (property #'id #'rhs 'private)]
                       [(#:constructor . _) (if (eq? key 'uses_default_constructor) '(#f) null)]
                       [(#:expression . _) (if (eq? key 'uses_default_constructor) '(#f) null)]
                       [(#:binding . _) (if (eq? key 'uses_default_binding) '(#f) null)]
                       [(#:annotation . _) (if (eq? key 'uses_default_annotation) '(#f) null)]
                       [_ null]))]
             [e (in-list v)])
    e))

(define-for-syntax (method-shape-extract shapes private-methods private-properties key)
  (define (unwrap a) (if (vector? a) (vector-ref a 0) a))
  (define (unshift-arity a) (and a (if (integer? a)
                                       (quotient a 2)
                                       (cons (quotient (car a) 2) (cdr a)))))
  (case key
    [(method_names)
     (append
      private-methods
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unwrap ma))]
                 #:unless (pair? m))
        (datum->syntax #f (if (box? m) (unbox m) m))))]
    [(method_arities)
     (append
      (for/list ([m (in-list private-methods)])
        #f)
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unwrap ma))]
                 #:unless (pair? m))
        (unshift-arity (and (vector? ma) (vector-ref ma 1)))))]
    [(method_visibilities)
     (append
      (for/list ([m (in-list private-methods)])
        'private)
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unwrap ma))]
                 #:unless (pair? m))
        'public))]
    [(property_names)
     (append
      private-properties
      (for ([ma (in-vector shapes)]
            #:do [(define m (unwrap ma))]
            #:when (pair? m))
        (let ([m (car m)])
          (datum->syntax #f (if (box? m) (unbox m) m)))))]
    [(property_arities)
     (append
      (for/list ([m (in-list private-methods)])
        #f)
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unwrap ma))]
                 #:when (pair? m))
        (unshift-arity (and (vector? ma) (vector-ref ma 1)))))]
    [(property_visibilities)
     (append
      (for/list ([m (in-list private-properties)])
        'private)
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unwrap ma))]
                 #:when (pair? m))
        'public))]))
