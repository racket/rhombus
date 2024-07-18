#lang racket/base
(require (for-syntax racket/base))

(provide bounce
         bounce-meta)

(define-syntax (bounce stx)
  (syntax-case stx ()
    [(_  #:except (ex ...) mod ...)
     (with-syntax ([(mod ...) ((make-syntax-introducer) #'(mod ...))])
       #'(begin (begin (require (except-in mod ex ...))
                       (provide (all-from-out mod)))
                ...))]
    [(_  #:only (ex ...) #:spaces (space ...) mod)
     (let ([intro (make-syntax-introducer)])
       (with-syntax ([mod (intro #'mod)]
                     [(ex ...) (intro #'(ex ...))])
         #'(begin (begin (require (only-space-in space (only-in mod ex ...)))
                         (provide (for-space space ex ...)))
                  ...)))]
    [(_ mod ...) #'(bounce #:except () mod ...)]))

(define-syntax (bounce-meta stx)
  (syntax-case stx ()
    [(_  #:only (ex ...) #:spaces (space ...) mod)
     (let ([intro (make-syntax-introducer)])
       (with-syntax ([mod (intro #'mod)]
                     [(ex ...) (intro #'(ex ...))])
         #'(begin (begin (require (only-meta-in 1 (only-in (only-space-in space mod) ex ...)))
                         (provide (for-syntax (for-space space ex ...))))
                  ...)))]
    [(_  mod ...)
     (with-syntax ([(mod ...) ((make-syntax-introducer) #'(mod ...))])
       #'(begin (begin (require (for-syntax mod))
                       (provide (for-syntax (all-from-out mod))))
                ...))]))
