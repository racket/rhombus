#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide bounce)

(define-syntax (bounce stx)
  (syntax-case stx ()
    [(_ mod ...)
     (with-syntax ([(mod ...) ((make-syntax-introducer) #'(mod ...))])
       #'(begin (begin (require mod)
                       (provide (all-from-out mod)))
                ...))]))
