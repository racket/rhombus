#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "expression.rkt"
         (submod "contract.rkt" for-struct)
         "static-info.rkt"
         "indexed-ref-set-key.rkt"
         "call-result-key.rkt"
         (only-in "assign.rkt"
                  [= rhombus=])
         "parse.rkt")

(provide Map
         (for-space rhombus/contract Map)
         (for-space rhombus/static-info Map)

         make_map
         (for-space rhombus/static-info make_map))

(define-syntax Map
  (expression-transformer
   #'Map
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (brackets block group op)
       [(_ (brackets (group key ... (op rhombus=) val ...) ...) . tail)
        (values #'(hash (~@ (rhombus-expression (group key ...))
                            (rhombus-expression (group val ...)))
                        ...)
                #'tail)]
       [(form-id (~and wrong (brackets . _)) . tail)
        (raise-syntax-error #f
                            "bad group within brackets"
                            (relocate (span-srcloc #'form-id #'wrong)
                                      #'(form-id wrong)))]
       [(_ . tail) (values #'Map-op
                           #'tail)]))))

(define Map-op
  (let ([Map (lambda args
               (apply hash args))])
    Map))
       
(define-contract-syntax Map
  (identifier-contract #'Map #'hash? #'((#%indexed-ref hash-ref)
                                        (#%indexed-set! hash-set!))))

(define-static-info-syntax Map
  (#%call-result ((#%indexed-ref hash-ref))))

(define-static-info-syntax Map-op
  (#%call-result ((#%indexed-ref hash-ref))))

(define (make_map . l)
  (hash-copy (apply hash l)))

(define-static-info-syntax make_map
  (#%call-result ((#%indexed-ref hash-ref)
                  (#%indexed-set! hash-set!))))
