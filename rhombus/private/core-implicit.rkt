#lang racket/base
(require (for-syntax racket/base
                     "srcloc.rkt"
                     "op.rkt"))

(provide #%tuple
         #%call)

(define-syntax #%tuple
  (rhombus-multi-prefix-operator (lambda (forms stx)
                                  (cond
                                    [(null? forms)
                                     (raise-syntax-error #f "empty expression" stx)]
                                    [(pair? (cdr forms))
                                     (raise-syntax-error #f "too many expressions" stx)]
                                    [else (car forms)]))))

(define-syntax #%call
  (rhombus-multi-infix-operator (lambda (rator rands stx)
                                   (datum->syntax (quote-syntax here)
                                                  (cons rator rands)
                                                  (span-srcloc rator stx)
                                                  stx))))
