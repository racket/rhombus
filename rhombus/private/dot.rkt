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
         "realm.rkt"
         "racket-class.rkt"
         "parens.rkt"
         "parse.rkt"
         "racket-class.rkt")

(provide |.|
         use_static_dot
         use_dynamic_dot)

(module+ for-dot-provider
  (begin-for-syntax
    (provide (property-out dot-provider)
             
             in-dot-provider-space

             wrap-dot-provider))
  (provide define-dot-provider-syntax
           #%dot-provider
           prop:field-name->accessor))

(begin-for-syntax
  (property dot-provider (handler))

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

(define-for-syntax (make-|.| strict?)
  (expression-infix-operator
   (quote-syntax |.|)
   '((default . stronger))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(dot::operator field:identifier . tail)
        (define (generic)
          (if strict?
              (raise-syntax-error #f
                                  "strict operator not supported for left-hand side"
                                  #'dot.name
                                  #f
                                  (list form1))
              (syntax-parse #'tail
                [((p::parens e ...) . tail)
                 (values #`(dot-lookup-by-name #,form1 'field #t (rhombus-expression e) ...)
                         #'tail)]
                [tail
                 (values #`(dot-lookup-by-name #,form1 'field #f)
                         #'tail)])))
        (syntax-parse form1
          [dp::dot-provider
           (define p (syntax-local-value* (in-dot-provider-space #'dp.id) dot-provider-ref))
           (define e ((dot-provider-handler p) form1 #'dot #'field))
           (if e
               (values e #'tail)
               (generic))]
          [_ (generic)])]
       [(dot::operator other . tail)
        (raise-syntax-error #f
                            "expected an identifier for a field name, but found something else"
                            #'dot.name
                            #f
                            (list #'other))]))
   'left))

(define-syntax |.| (make-|.| #f))

(define-syntaxes (use_static_dot use_dynamic_dot)
  (let ([mk (lambda (strict?)
              (definition-transformer
                (lambda (stx)
                  (syntax-parse stx
                    [(form-id)
                     #`((define-syntax #,(datum->syntax #'form-id '|.|) (make-|.| #,strict?)))]))))])
    (values (mk #t)
            (mk #f))))

(define-syntax (define-dot-provider-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-dot-provider-space #'id)
         rhs)]))

(define-values (prop:field-name->accessor field-name->accessor? field-name->accessor-ref)
  (make-struct-type-property 'field-name->accessor
                             (lambda (field-names info)
                               (define gen-acc (list-ref info 3))
                               (for/hasheq ([name (in-list field-names)]
                                            [i (in-naturals)])
                                 (values name
                                         (make-struct-field-accessor gen-acc i name))))))

(define (dot-lookup-by-name subj field fun? . args)
  (define ht (field-name->accessor-ref subj #f))
  (define (fail)
    (raise-arguments-error* field
                            rhombus-realm
                            "no such field"
                            "in value" subj))
  (cond
    [(object? subj) (object-dot-lookup subj field fun? args fail)]
    [(not ht) (fail)]
    [(hash-ref ht field #f)
     => (lambda (acc)
          (define val (acc subj))
          (if fun? (apply val args) val))]
    [else (fail)]))

