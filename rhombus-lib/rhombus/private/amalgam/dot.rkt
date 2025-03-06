#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     enforest/syntax-local
                     "operator-parse.rkt"
                     "statically-str.rkt"
                     "srcloc.rkt")
         "provide.rkt"
         "dot-space.rkt"
         "expression.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "dot-property.rkt"
         "dot-provider-key.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         "realm.rkt"
         "parse.rkt"
         "is-static.rkt"
         (submod "assign.rkt" for-assign)
         "order.rkt"
         "order-primitive.rkt")

(provide (for-spaces (#f
                      rhombus/repet
                      rhombus/annot)

                     |.|))

(module+ for-dot-provider
  (begin-for-syntax
    (provide (property-out dot-provider)

             in-dot-provider-space))
  (provide define-dot-provider-syntax
           prop:field-name->accessor
           prop:field-name->mutator
           curry-method))

(module+ for-builtin
  (provide set-builtin->accessor-ref!
           set-builtin->mutator-ref!))

(module+ for-syntax-meta
  (provide (for-syntax
            parse-dot-expr
            parse-dot-repet)))

(module+ for-external
  (provide dot-lookup-by-name
           dot-assign-by-name))

(begin-for-syntax
  (property dot-provider (handler))

  (define-syntax-class :dot-provider
    (pattern id:identifier
             #:when (syntax-local-value* (in-dot-provider-space #'id) dot-provider-ref))
    (pattern (~var ref-id (:static-info #'#%dot-provider))
             #:with id #'ref-id.val)))

(define-syntax |.|
  (expression-infix-operator
   (lambda () (order-quote member_access))
   '()
   'macro
   (lambda (form1 tail)
     (define more-static? (is-static-context/tail? tail))
     (parse-dot-expr form1 tail #:as-static? more-static?))
   'left))

(define-for-syntax (parse-dot-expr form1 tail
                                   #:as-static? [more-static? #f]
                                   #:no-generic? [no-generic? #f])
  (parse-dot-provider
   tail
   (lambda (dot dot-name field-id tail)
     (let ([form1 (rhombus-local-expand form1)])
       (define dp-id/s
         (syntax-parse form1
           [dp::dot-provider #'dp.id]
           [_ #f]))
       (build-dot-access form1 dp-id/s
                         more-static? #:repetition? #f
                         dot dot-name field-id tail
                         #:generic
                         (and no-generic?
                              (lambda () (values #f #f))))))))

(define-repetition-syntax |.|
  (repetition-infix-operator
   (lambda () (order-quote member_access))
   '()
   'macro
   (lambda (form1 tail)
     (define more-static? (is-static-context/tail? tail))
     (parse-dot-repet form1 tail #:as-static? more-static?))
   'left))

(define-for-syntax (parse-dot-repet form1 tail
                                    #:as-static? more-static?
                                    #:no-generic? [no-generic? #f])
  (parse-dot-provider
   tail
   (lambda (dot dot-name field-id tail)
     (syntax-parse form1
       [rep::repetition-info
        (define dp-id/s
          (or (repetition-static-info-lookup #'rep.element-static-infos #'#%dot-provider)
              (and (null? (syntax->list #'rep.for-clausess))
                   (identifier? #'rep.body)
                   (syntax-local-value* (in-dot-provider-space #'rep.body) dot-provider-ref))))
        (build-dot-access
         form1 dp-id/s
         more-static? #:repetition? #t
         dot dot-name field-id tail
         #:generic
         (if no-generic?
             (lambda () (values #f #f))
             (lambda ()
               (define rep
                 (build-compound-repetition
                  (datum->syntax #f (list form1 dot field-id)) (list form1)
                  (lambda (form1)
                    (define-values (expr new-tail)
                      (build-dot-access form1 dp-id/s
                                        more-static? #:for-repetition? #t
                                        dot dot-name field-id tail))
                    (set! tail new-tail)
                    (values (discard-static-infos expr)
                            (extract-static-infos expr)))))
               (values rep
                       tail))))]))))

;; annotation, declared explicitly to create a syntax error
;; for something like `"a" :: String.length()`
(define-annotation-syntax |.|
  (annotation-infix-operator
   (lambda () (order-quote member_access))
   '()
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(name . _)
        (raise-syntax-error #f
                            "not an annotation operator"
                            #'name)]))
   'left))

(define-for-syntax (parse-dot-provider tail finish)
  (syntax-parse tail
    [(dot::operator field:identifier . tail)
     (finish #'dot #'dot.name #'field #'tail)]
    [(dot::operator other . tail)
     (raise-syntax-error #f
                         "expected an identifier for a field or method name, but found something else"
                         #'dot.name
                         #f
                         (list #'other))]
    [(dot:identifier . tail)
     ;; wrap as an operator
     (parse-dot-provider #'((op dot) . tail) finish)]))

(define-for-syntax (build-dot-access form1 dp-id/s
                                     more-static?
                                     dot dot-name field-id tail
                                     #:repetition? [repetition? #f]
                                     #:for-repetition? [for-repetition? #f]
                                     #:generic [alt-generic #f])
  (define (generic)
    (cond
      [alt-generic (alt-generic)]
      [more-static?
       (raise-syntax-error #f
                           (string-append "no such field or method" statically-str)
                           field-id)]
      [else
       (define (lookup)
         (values (relocate+reraw (datum->syntax #f (list form1 dot field-id))
                                 #`(dot-lookup-by-name #,form1 '#,field-id))
                 tail))
       (cond
         [for-repetition? (lookup)]
         [else
          (syntax-parse tail
            [assign::assign-op-seq
             #:do [(define op (attribute assign.op))]
             (define-values (assign-expr tail) (build-assign
                                                op
                                                #'assign.op-name
                                                #'assign.name
                                                #`(lambda () (dot-lookup-by-name lhs '#,field-id))
                                                #`(lambda (v) (dot-assign-by-name lhs '#,field-id v))
                                                #'field-id
                                                #'assign.tail))
             (values #`(let ([lhs #,form1])
                         #,assign-expr)
                     tail)]
            [_ (lookup)])])]))
  (define dp-ids (extract-dot-provider-ids dp-id/s))
  (let loop ([dp-ids dp-ids])
    (cond
      [(null? dp-ids) (generic)]
      [else
       (define dp-id (car dp-ids))
       (define p (syntax-local-value* (in-dot-provider-space dp-id) dot-provider-ref))
       (unless p (raise-syntax-error #f "not bound as a dot provider" (in-dot-provider-space dp-id)))
       (define (success-k expr tail)
         (values expr tail))
       ((dot-provider-handler p) form1 dot field-id
                                 tail
                                 more-static?
                                 repetition?
                                 success-k (lambda ()
                                             (loop (cdr dp-ids))))])))

(define-syntax (define-dot-provider-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-dot-provider-space #'id)
         rhs)]))

;; To tie a loop with built-in data structures:
(define builtin->accessor-ref (lambda (v) #f))
(define builtin->mutator-ref (lambda (v) #f))
(define (set-builtin->accessor-ref! proc) (set! builtin->accessor-ref proc))
(define (set-builtin->mutator-ref! proc) (set! builtin->mutator-ref proc))

(define (dot-lookup-by-name v field)
  (define ht (or (field-name->accessor-ref v #f)
                 (builtin->accessor-ref v)))
  (define (fail)
    (raise-arguments-error* field
                            rhombus-realm
                            "no such field or method"
                            "in value" v))
  (cond
    [(not ht) (fail)]
    [(pair? ht) (or
                 (let loop ([ht ht])
                   (cond
                     [(pair? ht) (or (loop (car ht)) (loop (cdr ht)))]
                     [(hash-ref ht field #f) => (lambda (acc) (acc v))]
                     [else #f]))
                 (fail))]
    [(hash-ref ht field #f) => (lambda (acc) (acc v))]
    [else (fail)]))

(define (dot-assign-by-name v field new-val)
  (define ht (or (field-name->mutator-ref v #f)
                 (builtin->mutator-ref v)))
  (define (fail)
    (raise-arguments-error* field
                            rhombus-realm
                            "no such mutable field or property"
                            "in value" v))
  (cond
    [(not ht) (fail)]
    [(hash-ref ht field #f) => (lambda (mut) (mut v new-val))]
    [else (fail)]))
