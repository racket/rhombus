#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide bounce
         bounce-meta)

(define-syntax (bounce stx)
  (syntax-case stx ()
    [(_  #:except (ex ...) mod ...)
     (with-syntax ([(mod ...) ((make-syntax-introducer) #'(mod ...))])
       #'(begin (begin (require (except-in mod ex ...))
                       (provide (all-from-out mod)))
                ...))]
    [(_ mod ...) #'(bounce #:except () mod ...)]))

(define-syntax (bounce-meta stx)
  (syntax-case stx ()
    [(_  mod ...)
     (with-syntax ([(mod ...) ((make-syntax-introducer) #'(mod ...))])
       #'(begin (begin (require (for-syntax mod))
                       (provide (for-syntax (all-from-out mod))))
                ...))]))
