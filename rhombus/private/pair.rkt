#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx
                     "srcloc.rkt")
         "provide.rkt"
         "composite.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "reducer.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "name-root.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parse.rkt"
         "dot-parse.rkt"
         "realm.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Pair))

(module+ for-builtin
  (provide pair-method-table))

(define pair-method-table
  (hash 'first car
        'rest cdr))

(define-for-syntax pair-static-infos
  #'((#%dot-provider pair-instance)))

(define-for-syntax pair-binding
  (make-composite-binding-transformer "Pair"
                                      #'pair?
                                      #:static-infos #'((#%dot-provider pair-instance))
                                      (list #'car #'cdr)
                                      (list #'() #'())))

(define-binding-syntax cons (binding-transformer
                             pair-binding))

(define-name-root Pair
  #:fields
  (cons
   [first car]
   [rest cdr]
   of))

(define-syntax Pair
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(head . tail) (values (relocate-id #'head #'cons) #'tail)]))))

(define-binding-syntax Pair
  (binding-transformer
   pair-binding))

(define-annotation-constructor (Pair of)
  ()
  #'pair? pair-static-infos
  2
  #f
  (lambda (arg-id predicate-stxs)
    #`(and (#,(car predicate-stxs) (car #,arg-id))
           (#,(cadr predicate-stxs) (cdr #,arg-id))))
  (lambda (static-infoss)
    #`((#%first-result #,(car static-infoss))
       (#%rest-result #,(cadr static-infoss)))))

(define-syntax pair-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(first) (field (lambda (x) (add-info #`(car #,x) x #'#%first-result)))]
        [(rest) (field (lambda (x) (add-info #`(cdr #,x) x #'#%rest-result)))]
        [else #f])))))

(define-for-syntax (add-info e on-e key)
  (define result-static-infos (syntax-local-static-info on-e key))
  (if result-static-infos
      (wrap-static-info* e result-static-infos)
      e))

(define-static-info-syntax cons
  (#%call-result #,pair-static-infos)
  (#%function-arity 4))
