#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         "binding.rkt"
         (submod "contract.rkt" for-struct)
         "static-info.rkt"
         "indexed-ref-set-key.rkt"
         "call-result-key.rkt"
         "composite.rkt")

(provide Array
         (for-space rhombus/binding Array)
         (for-space rhombus/contract Array)
         (for-space rhombus/static-info Array))

(define-syntax Array
  (lambda (stx)
    (syntax-parse stx
      #:literals (list)
      [(_ (list e ...)) #'(vector e ...)]
      [(_ args ...) (syntax/loc stx #'(Array-proc args ...))]
      [_:identifier (datum->syntax stx 'Array-proc stx stx)])))

(define Array-proc
  (let ([Array (lambda (l)
                 (if (list? l)
                     (apply vector l)
                     (raise-argument-error 'Array
                                           "list?"
                                           l)))])
    Array))

(define-contract-syntax Array
  (identifier-contract #'Array #'vector? #'((#%indexed-ref vector-ref)
                                            (#%indexed-set! vector-set!))))

(define-static-info-syntax Array
  (#%call-result ((#%indexed-ref vector-ref)
                  (#%indexed-set! vector-set!))))

(define-binding-syntax Array
  (binding-prefix-operator
   #'Array
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(form-id ((~and tag (~datum parens)) arg ...) . tail)
        (define args (syntax->list #'(arg ...)))
        (define len (length args))
        (define pred #`(lambda (v)
                         (and (vector? v)
                              (= (vector-length v) #,len))))
        ((make-composite-binding-transformer pred
                                             (for/list ([arg (in-list args)]
                                                        [i (in-naturals)])
                                               #`(lambda (v) (vector-ref v #,i)))
                                             (for/list ([arg (in-list args)])
                                               #'()))
         stx)]))))
