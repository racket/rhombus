#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/property
                     enforest/syntax-local
                     "operator-parse.rkt")
         "definition.rkt"
         "expression.rkt"
         "static-info.rkt"
         "dot-provider-key.rkt"
         "realm.rkt")

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
           curry-method))

(module+ for-builtin
  (provide set-builtin->accessor-ref!))

(module+ for-dynamic-static
  (provide (for-syntax make-|.|)))

(begin-for-syntax
  (property dot-provider (handler))
  (property dot-provider-more-static dot-provider ())

  (define in-dot-provider-space (make-interned-syntax-introducer 'rhombus/dot-provider))

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
  (expression-infix-operator
   (quote-syntax |.|)
   '((default . stronger))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(dot::operator field:identifier . tail)
        (define (generic)
          (if more-static?
              (raise-syntax-error #f
                                  "static operator not supported for left-hand side"
                                  #'dot.name
                                  #f
                                  (list form1))
              (values #`(dot-lookup-by-name #,form1 'field)
                      #'tail)))
        (syntax-parse form1
          [dp::dot-provider
           (define p (syntax-local-value* (in-dot-provider-space #'dp.id) dot-provider-ref))
           (if (dot-provider-more-static? p)
               ((dot-provider-handler p) form1 #'dot #'field
                                         #'tail
                                         more-static?
                                         values generic)
               (let ([e ((dot-provider-handler p) form1 #'dot #'field)])
                 (if e
                     (values e #'tail)
                     (generic))))]
          [_ (generic)])]
       [(dot::operator other . tail)
        (raise-syntax-error #f
                            "expected an identifier for a field name, but found something else"
                            #'dot.name
                            #f
                            (list #'other))]))
   'left))

(define-syntax |.| (make-|.| #f))

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
                               

;; To tie a loop with built-in data structures:
(define builtin->accessor-ref (lambda (v) #f))
(define (set-builtin->accessor-ref! proc) (set! builtin->accessor-ref proc))

(define (dot-lookup-by-name v field)
  (define ht (or (field-name->accessor-ref v #f)
                 (builtin->accessor-ref v)))
  (define (fail)
    (raise-arguments-error* field
                            rhombus-realm
                            "no such field"
                            "in value" v))
  (cond
    [(not ht) (fail)]
    [(hash-ref ht field #f) => (lambda (acc) (acc v))]
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
