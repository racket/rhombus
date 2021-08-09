#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         (submod "contract.rkt" for-struct)
         "static-info.rkt"
         "indexed-ref-set-key.rkt"
         "call-result-key.rkt")

(provide Array
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
