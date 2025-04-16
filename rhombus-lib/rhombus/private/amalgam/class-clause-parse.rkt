#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "class-parse.rkt"
                     "expose.rkt")
         (submod "annotation.rkt" for-class)
         "entry-point.rkt"
         "parens.rkt"
         (only-in "syntax-parameter.rkt"
                  syntax-parameters-key)
         (only-in "function-arity.rkt"
                  shift-arity)
         "extract-rhs.rkt")

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

(define-for-syntax (parse-annotation-options orig-stx forms stx-paramss)
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
              [(#:implements id ...)
               (add-implements options 'public-implements #'(id ...))]
              [(#:private-implements id ...)
               (add-implements options 'private-implements #'(id ...))]
              [(#:protected-implements id ...)
               (add-implements options 'protected-implements #'(id ...))]
              [(#:internal id)
               (hash-set options 'internals (cons #'id (hash-ref options 'internals '())))]
              [(#:annotation block)
               (when (hash-has-key? options 'annotation-rhs)
                 (raise-syntax-error #f "multiple annotation clauses" orig-stx clause))
               (hash-set options 'annotation-rhs (extract-rhs #'block))]
              [(#:expression block/none)
               ;; needed to determine whether default `.of` annotation can work
               (hash-set options 'expression-rhs #t)]
              [(#:nonfinal)
               ;; needed for whether to support converting field annotations;
               ;; leave checking to `parse-options'
               (hash-set options 'final? #f)]
              [(#:prefab)
               ;; needed for whether to support converting field annotations;
               ;; leave checking to `parse-options'
               (hash-set options 'prefab? #t)]
              [(#:converter)
               ;; needed to define a veneer annotation;
               ;; leave checking to `parse-options'
               (hash-set options 'converter? #t)]
              [(#:field mutability . _)
               ;; needed for whether to support converting field annotations;
               ;; leave general handling to `parse-options'
               (if (eq? 'mutable (syntax-e #'mutability))
                   (hash-set options 'has-mutable-field? #t)
                   options)]
              [(#:static-infos expr)
               (hash-set options 'static-infoss (cons #'expr (hash-ref options 'static-infoss '())))]
              [_ options]))
          (loop (cdr clauses) new-options)]))]))

(define-for-syntax (parse-options orig-stx forms stx-paramss)
  (syntax-parse forms
    #:context orig-stx
    [((_ clause-parsed) ...)
     (define clauses (syntax->list #'(clause-parsed ...)))
     (let loop ([clauses clauses]
                [stx-paramss (syntax->list stx-paramss)]
                [options #hasheq()])
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
              [(#:protected-implements id ...)
               (add-implements options 'protected-implements #'(id ...))]
              [(#:internal id)
               (hash-set options 'internals (cons #'id (hash-ref options 'internals '())))]
              [(#:constructor id forward-rets rhs)
               (when (hash-has-key? options 'constructor-rhs)
                 (raise-syntax-error #f "multiple constructor clauses" orig-stx clause))
               (define rhs-options (hash-set (hash-set options 'constructor-rhs #'rhs)
                                             'constructor-stx-params (car stx-paramss)))
               (define rhs+name-options
                 (if (syntax-e #'id)
                     (hash-set rhs-options 'constructor-name #'id)
                     rhs-options))
               (if (syntax-e #'forward-rets)
                   (hash-set rhs+name-options 'constructor-forward-rets #'forward-rets)
                   rhs+name-options)]
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
              [(#:reconstructor block)
               (when (hash-has-key? options 'reconstructor-rhs)
                 (raise-syntax-error #f "multiple reconstructor clauses" orig-stx clause))
               (hash-set (hash-set options 'reconstructor-rhs (extract-rhs #'block))
                         'reconstructor-stx-params (car stx-paramss))]
              [(#:reconstructor_fields orig-id ids rhss)
               (when (hash-has-key? options 'reconstructor-fields)
                 (raise-syntax-error #f "multiple reconstructor-fields clauses" orig-stx clause))
               (hash-set options 'reconstructor-fields #'(orig-id ids rhss))]
              [(#:nonfinal)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "multiple finality clauses" orig-stx clause))
               (hash-set options 'final? #f)]
              [(#:authentic)
               (when (hash-has-key? options 'authentic?)
                 (raise-syntax-error #f "multiple authenticity clauses" orig-stx clause))
               (when (hash-ref options 'prefab? #f)
                 (raise-syntax-error #f "a prefab class cannot be authentic" orig-stx clause))
               (hash-set options 'authentic? #t)]
              [(#:prefab)
               (when (hash-has-key? options 'prefab?)
                 (raise-syntax-error #f "multiple prefab clauses" orig-stx clause))
               (when (hash-ref options 'authentic? #f)
                 (raise-syntax-error #f "an authentic class cannot be prefab" orig-stx clause))
               (when (hash-ref options 'opaque? #f)
                 (raise-syntax-error #f "an opaque class cannot be prefab" orig-stx clause))
               (hash-set options 'prefab? #t)]
              [(#:opaque)
               (when (hash-has-key? options 'opaque?)
                 (raise-syntax-error #f "multiple opacity clauses" orig-stx clause))
               (when (hash-has-key? options 'prefab?)
                 (raise-syntax-error #f "a prefab class cannot be opaque" orig-stx clause))
               (hash-set options 'opaque? #t)]
              [(#:converter)
               (when (hash-has-key? options 'converter?)
                 (raise-syntax-error #f "multiple converter clauses" orig-stx clause))
               (hash-set options 'converter? #t)]
              [(#:static-infos expr)
               ;; covered in annotation pass
               options]
              [(#:field mutability id rhs-id ann-seq default form-id mode)
               (with-syntax ([(converter annotation-str static-infos)
                              (with-continuation-mark
                               syntax-parameters-key (car stx-paramss)
                               (syntax-parse #'ann-seq
                                 [#f (list #'#f #'#f #'())]
                                 [(c::inline-annotation)
                                  (list #'c.converter #'c.annotation-str #'c.static-infos)]))])
                 (hash-set options 'fields (cons (added-field #'id
                                                              #'rhs-id #'default (car stx-paramss) #'form-id
                                                              #'static-infos
                                                              #'converter
                                                              #'annotation-str
                                                              (syntax-e #'mode)
                                                              (syntax-e #'mutability))
                                                 (hash-ref options 'fields null))))]
              [(#:primitive-property prop-id val-id)
               (hash-set options 'primitive-properties
                         (cons (cons #'prop-id #'val-id)
                               (hash-ref options 'primitive-properties null)))]
              [(#:serializable form-id version-n serialize-rhs deserialize-rhs deserialize-shell-rhs deserialize-fill-rhs)
               (when (hash-has-key? options 'converter?)
                 (raise-syntax-error #f "multiple serialization clauses" orig-stx #'form-id))
               (hash-set (hash-set options 'serializable #`(form-id version-n
                                                                    #,(and (syntax-e #'serialize-rhs)
                                                                           (extract-rhs #'serialize-rhs))
                                                                    #,(and (syntax-e #'deserialize-rhs)
                                                                           (extract-rhs #'deserialize-rhs))
                                                                    #,(and (syntax-e #'deserialize-shell-rhs)
                                                                           (extract-rhs #'deserialize-shell-rhs))
                                                                    #,(and (syntax-e #'deserialize-fill-rhs)
                                                                           (extract-rhs #'deserialize-fill-rhs))))
                         'serializer-stx-params (car stx-paramss))]
              [_
               (parse-method-clause orig-stx options clause (car stx-paramss))]))
          (loop (cdr clauses) (cdr stx-paramss) new-options)]))]))

(define-for-syntax (parse-method-clause orig-stx options clause stx-params)
  (syntax-parse clause
    [((~and tag (~or* #:method #:override #:private #:protected #:final #:final-override #:final-protected #:private-override
                      #:property #:override-property #:protected-property
                      #:final-property #:final-override-property #:final-protected-property
                      #:private-property #:private-override-property))
      id rhs has-cases? maybe-ret)
     (define-values (body replace disposition exposure kind)
       (case (syntax-e #'tag)
         [(#:method) (values 'method 'method 'abstract 'public 'method)]
         [(#:override) (values 'method 'override 'abstract 'public 'method)]
         [(#:private) (values 'method 'method 'private 'private 'method)]
         [(#:private-override) (values 'method 'override 'private 'private 'method)]
         [(#:protected) (values 'method 'method 'public 'protected 'method)]
         [(#:final) (values 'method 'method 'final 'public 'method)]
         [(#:final-override) (values 'method 'override 'final 'public 'method)]
         [(#:final-protected) (values 'method 'method 'final 'protected 'method)]
         [(#:property) (values 'method 'method 'abstract 'public 'property)]
         [(#:override-property) (values 'method 'override 'abstract 'public 'property)]
         [(#:protected-property) (values 'method 'method 'public 'protected 'property)]
         [(#:final-property) (values 'method 'method 'final 'public 'property)]
         [(#:final-override-property) (values 'method 'override 'final 'public 'property)]
         [(#:final-protected-property) (values 'method 'method 'final 'protected 'property)]
         [(#:private-property) (values 'method 'method 'private 'private 'property)]
         [(#:private-override-property) (values 'method 'override 'private 'private 'property)]
         [else (error "method kind not handled" #'tag)]))
     (define-values (arity reflect-name) (extract-shape #'rhs))
     (hash-set options 'methods (cons (added-method #'id
                                                    (car (generate-temporaries #'(id)))
                                                    #'rhs
                                                    stx-params
                                                    (syntax-e #'has-cases?)
                                                    #'maybe-ret
                                                    (and (or (syntax-parse #'maybe-ret
                                                               [(args (t . _)) #t]
                                                               [_ #f])
                                                             arity
                                                             ;; may need to propagate super:
                                                             (eq? replace 'override))
                                                         (car (generate-temporaries #'(id))))
                                                    body
                                                    replace
                                                    disposition
                                                    exposure
                                                    kind
                                                    arity
                                                    reflect-name)
                                      (hash-ref options 'methods null)))]
    [((~and tag (~or* #:abstract #:abstract-protected
                      #:abstract-property #:abstract-protected-property
                      #:abstract-override #:abstract-override-property))
      id rhs has-cases? maybe-ret)
     (define-values (replace exposure kind)
       (case (syntax-e #'tag)
         [(#:abstract) (values 'method 'public 'method)]
         [(#:abstract-protected) (values 'method 'protected 'method)]
         [(#:abstract-property) (values 'method 'public 'property)]
         [(#:abstract-protected-property) (values 'method 'protected 'property)]
         [(#:abstract-override) (values 'override 'public 'method)]
         [(#:abstract-override-property) (values 'override 'public 'property)]
         [else (error "method kind not handled" #'tag)]))
     (define-values (arity reflect-name) (extract-shape #'rhs))
     (hash-set options 'methods (cons (added-method #'id
                                                    '#:abstract
                                                    #'rhs
                                                    stx-params
                                                    #f
                                                    #'maybe-ret
                                                    (and (or (pair? (syntax-e #'maybe-ret))
                                                             arity
                                                             ;; may need to propagate super:
                                                             (eq? replace 'override))
                                                         (car (generate-temporaries #'(id))))
                                                    'abstract
                                                    replace
                                                    'abstract
                                                    exposure
                                                    kind
                                                    arity
                                                    reflect-name)
                                      (hash-ref options 'methods null)))]
    [_
     (raise-syntax-error #f "unrecognized clause" orig-stx clause)]))

(define-for-syntax (add-implements options extra-key ids-stx)
  (define l (reverse (syntax->list ids-stx)))
  (define new-options
    (hash-set options 'implements (append l (hash-ref options 'implements '()))))
  (if extra-key
      (hash-set new-options extra-key (append l (hash-ref new-options extra-key '())))
      new-options))

(define-for-syntax (extract-shape rhs)
  (syntax-parse rhs
    [(_ e-arity::entry-point-shape)
     (define ht (syntax-e #'e-arity.parsed))
     (cond
       [ht (values
            (let ([a (hash-ref (syntax-e #'e-arity.parsed) 'arity #f)])
              (and a
                   (let ([a (syntax->datum a)])
                     (if (exact-integer? a)
                         (* 2 a)
                         (cons (* 2 (car a)) (cdr a))))))
            (hash-ref ht 'name #f))]
       [else (values #f #f)])]
    [_ (values #f #f)]))

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
      [(method_names) (list (or (extract-name rhs) id))]
      [(method_arities) (list (extract-arity rhs))]
      [(method_visibilities) (list vis)]
      [else null]))
  (define (property id rhs vis)
    (case key
      [(property_names) (list (or (extract-name rhs) id))]
      [(property_arities) (list (extract-arity rhs))]
      [(property_visibilities) (list vis)]
      [else null]))
  (define (extract-shape e key)
    (syntax-parse e
      [(_ e-arity::entry-point-shape)
       (define ht (syntax->datum #'e-arity.parsed))
       (and ht (hash-ref ht key #f))]
      [_ #f]))
  (define (extract-arity e)
    (extract-shape e 'arity))
  (define (extract-name e)
    (extract-shape e 'name))
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
                       [(#:protected-implements id ...) (case key
                                                          [(implements)
                                                           (syntax->list #'(id ...))]
                                                          [(implements_visibilities)
                                                           '(protected)]
                                                          [else null])]
                       [(#:field mutability id rhs-id ann-seq default form-id mode)
                        (case key
                          [(field-names) (list #'id)]
                          [(field-visibilities) (list #'mode)]
                          [(field-mutabilities) (list #'mutability)]
                          [else null])]
                       [(#:internal id) (case key
                                          [(internal_names) (list #'id)]
                                          [else null])]
                       [(#:method id rhs . _) (method #'id #'rhs 'public)]
                       [(#:override id rhs . _) (method #'id #'rhs 'public)]
                       [(#:protected id rhs . _) (method #'id #'rhs 'protected)]
                       [(#:private id rhs . _) (method #'id #'rhs 'private)]
                       [(#:private-override id rhs . _) (method #'id #'rhs 'private)]
                       [(#:final id rhs . _) (method #'id #'rhs 'public)]
                       [(#:final-override id rhs . _) (method #'id #'rhs 'public)]
                       [(#:property id rhs . _) (property #'id #'rhs 'public)]
                       [(#:protected-property id rhs . _) (property #'id #'rhs 'protected)]
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
  (define (unprotect a) (if (protect? a) (protect-v a) a))
  (define (unshift-arity a) (and a (shift-arity a -1)))
  (case key
    [(method_names)
     (append
      private-methods
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unprotect (unwrap ma)))]
                 #:unless (pair? m))
        (datum->syntax #f (if (box? m) (unbox m) m))))]
    [(method_arities)
     (append
      (for/list ([m (in-list private-methods)])
        #f)
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unprotect (unwrap ma)))]
                 #:unless (pair? m))
        (unshift-arity (and (vector? ma) (vector-ref ma 1)))))]
    [(method_visibilities)
     (append
      (for/list ([m (in-list private-methods)])
        'private)
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unwrap ma))]
                 #:unless (pair? (unprotect m)))
        (if (protect? m)
            'protected
            'public)))]
    [(property_names)
     (append
      private-properties
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unprotect (unwrap ma)))]
                 #:when (pair? m))
        (let ([m (car m)])
          (datum->syntax #f (if (box? m) (unbox m) m)))))]
    [(property_arities)
     (append
      (for/list ([m (in-list private-properties)])
        #f)
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unprotect (unwrap ma)))]
                 #:when (pair? m))
        (unshift-arity (and (vector? ma) (vector-ref ma 1)))))]
    [(property_visibilities)
     (append
      (for/list ([m (in-list private-properties)])
        'private)
      (for/list ([ma (in-vector shapes)]
                 #:do [(define m (unwrap ma))]
                 #:when (pair? (unprotect m)))
        (if (protect? m)
            'protected
            'public)))]))
