#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide bounce)

(define-syntax (bounce stx)
  (syntax-case stx ()
    [(_  #:except (ex ...) mod ...)
     (with-syntax ([(mod ...) ((make-syntax-introducer) #'(mod ...))])
       #'(begin (begin (require (except-in mod ex ...))
                       (provide (all-from-out mod)))
                ...))]
    [(_ mod ...) #'(bounce #:except () mod ...)]))
