#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     enforest/syntax-local
                     "operator-parse.rkt"
                     "tag.rkt"
                     "statically-str.rkt")
         "definition.rkt"
         "expression.rkt"
         "expression+annotation.rkt"
         "static-info.rkt"
         "dot-provider-key.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         "realm.rkt"
         "parse.rkt"
         "assign.rkt")

(provide |.|)

(module+ for-dot-provider
  (begin-for-syntax
    (provide (property-out dot-provider)
             (property-out dot-provider-more-static)
             
             in-dot-provider-space

             wrap-dot-provider))
  (provide define-dot-provider-syntax
           #%dot-provider
           prop:field-name->accessor
           prop:field-name->mutator
           curry-method))

(module+ for-builtin
  (provide set-builtin->accessor-ref!))

(module+ for-dynamic-static
  (provide |.|
           static-|.|))

(begin-for-syntax
  (property dot-provider (handler))
  (property dot-provider-more-static dot-provider ())

  (define in-dot-provider-space (make-interned-syntax-introducer 'rhombus/dot))

  (define (wrap-dot-provider expr provider-stx)
    (quasisyntax/loc expr
      (begin (quote-syntax (#%dot-provider #,provider-stx))
             #,expr)))

  (define-syntax-class :dot-provider
    #:literals (begin quote-syntax #%dot-provider)
    (pattern id:identifier
             #:when (syntax-local-value* (in-dot-provider-space #'id) dot-provider-ref))
    (pattern (~var ref-id (:static-info #'#%dot-provider))
             #:attr id #'ref-id.val)))

(define-for-syntax (make-|.| more-static?)
  (make-expression+repetition+annotation-infix-operator
   (quote-syntax |.|)
   '((default . stronger))
   'macro
   ;; expression
   (lambda (form1 tail)
     (parse-dot-provider
      tail
      (lambda (dot dot-name field-id tail)
        (let ([form1 (rhombus-local-expand form1)])
          (define dp-id
            (syntax-parse form1
              [dp::dot-provider #'dp.id]
              [_ #f]))
          (build-dot-access form1 dp-id
                            more-static? #:repetition? #f
                            dot dot-name field-id tail)))))
   ;; repetition
   (lambda (form1 tail)
     (parse-dot-provider
      tail
      (lambda (dot dot-name field-id tail)
        (syntax-parse form1
          [rep::repetition-info
           (define dp-id
             (or (repetition-static-info-lookup #'rep.element-static-infos #'#%dot-provider)
                 (and (zero? (syntax-e #'rep.bind-depth))
                      (identifier? #'rep.seq-expr)
                      (syntax-local-value* (in-dot-provider-space #'rep.seq-expr) dot-provider-ref))))
           (define rep
             (build-compound-repetition
              dot (list form1)
              (lambda (form1)
                (define-values (expr new-tail)
                  (build-dot-access form1 dp-id
                                    more-static? #:repetition? #t
                                    dot dot-name field-id tail))
                (set! tail new-tail)
                (values expr
                        (extract-static-infos expr)))))
           (values rep
                   tail)]))))
   ;; annotation, declared explicitly to create a syntax error
   ;; for something like `"a" :: String.length()`
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
                         (list #'other))]))

(define-for-syntax (build-dot-access form1 dp-id
                                     more-static? #:repetition? repetition?
                                     dot dot-name field-id tail)
  (define (generic)
    (if more-static?
        (raise-syntax-error #f
                            (string-append "no such field or method" statically-str)
                            field-id)
        (syntax-parse tail
          #:datum-literals (op)
          #:literals (:=)
          [((op :=) . tail)
           #:when (not repetition?)
           #:with (~var e (:infix-op+expression+tail #':=)) #'(group . tail)
           (values #`(dot-assign-by-name #,form1 '#,field-id e.parsed)
                   #'e.tail)]
          [else
           (values #`(dot-lookup-by-name #,form1 '#,field-id)
                   tail)])))
  (cond
    [dp-id
     (define p (syntax-local-value* (in-dot-provider-space dp-id) dot-provider-ref))
     (unless p (raise-syntax-error #f "not bound as a dot provider" (in-dot-provider-space dp-id)))
     (if (dot-provider-more-static? p)
         ((dot-provider-handler p) form1 dot field-id
                                   tail
                                   more-static?
                                   values generic)
         (let ([e ((dot-provider-handler p) form1 dot field-id)])
           (if e
               (values e tail)
               (generic))))]
    [else (generic)]))


(define-syntax |.| (make-|.| #f))
(define-syntax static-|.| (make-|.| #t))

(define-syntax (define-dot-provider-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-dot-provider-space #'id)
         rhs)]))

(define-values (prop:field-name->accessor field-name->accessor? field-name->accessor-ref)
  (make-struct-type-property 'field-name->accessor
                             (lambda (field-names+ht+method-ht info)
                               (define field-names (car field-names+ht+method-ht))
                               (define gen-acc (list-ref info 3))
                               (define field-ht
                                 (for/fold ([ht (cadr field-names+ht+method-ht)]) ([name (in-list field-names)]
                                                                                   [i (in-naturals)])
                                   (hash-set ht name (make-struct-field-accessor gen-acc i name))))
                               (for/fold ([ht field-ht]) ([(name proc) (in-hash (cddr field-names+ht+method-ht))])
                                 (hash-set ht name (lambda (obj) (curry-method proc obj)))))))

(define-values (prop:field-name->mutator field-name->mutator? field-name->mutator-ref)
  (make-struct-type-property 'field-name->mutator
                             (lambda (field-names+ht info)
                               (define field-names (car field-names+ht))
                               (define gen-mut (list-ref info 4))
                               (for/fold ([ht (cdr field-names+ht)]) ([name (in-list field-names)]
                                                                      [i (in-naturals)]
                                                                      #:when name)
                                 (hash-set ht name (make-struct-field-mutator gen-mut i name))))))

;; To tie a loop with built-in data structures:
(define builtin->accessor-ref (lambda (v) #f))
(define (set-builtin->accessor-ref! proc) (set! builtin->accessor-ref proc))

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
    [(hash-ref ht field #f) => (lambda (acc) (acc v))]
    [else (fail)]))

(define (dot-assign-by-name v field new-val)
  (define ht (field-name->mutator-ref v #f))
  (define (fail)
    (raise-arguments-error* field
                            rhombus-realm
                            "no such mutable field or property"
                            "in value" v))
  (cond
    [(not ht) (fail)]
    [(hash-ref ht field #f) => (lambda (mut) (mut v new-val))]
    [else (fail)]))

(define (curry-method proc obj)
  (define-values (req-kws allowed-kws) (procedure-keywords proc))
  (cond
    [(null? allowed-kws)
     (procedure-reduce-arity-mask (lambda args
                                    (apply proc obj args))
                                  (arithmetic-shift (procedure-arity-mask proc) -1)
                                  (object-name proc))]
    [(null? allowed-kws)
     (procedure-reduce-keyword-arity-mask (make-keyword-procedure
                                           (lambda (kws kw-args . args)
                                             (keyword-apply proc kws kw-args obj args)))
                                          (arithmetic-shift (procedure-arity-mask proc) -1)
                                          req-kws
                                          allowed-kws
                                          (object-name proc))]))
