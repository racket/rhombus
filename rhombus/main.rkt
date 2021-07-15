#lang racket/base
(require (for-syntax racket/base)
         "parse.rkt")

(provide (rename-out [rhombus-module-begin #%module-begin]))

(define-syntax-rule (bounce mod ...)
  (begin (begin (require mod)
                (provide (all-from-out mod)))
         ...))
(bounce "private/core-implicit.rkt"
        "private/core-op.rkt"
        "private/struct.rkt"
        "private/define.rkt"
        "private/type.rkt"
        "private/list.rkt")

(module reader racket/base
  (require shrubbery/parse
           (only-in (submod shrubbery reader)
                    get-info))
  
  (provide (rename-out [rhombus-read read]
                       [rhombus-read-syntax read-syntax])
           get-info)

  (define (rhombus-read in)
    (syntax->datum
     (rhombus-read-syntax #f in)))
 
  (define (rhombus-read-syntax src in)
    (define r (parse-all in))
    (if (eof-object? r)
        r
        (datum->syntax
         #f
         `(module anything rhombus
            (#%module-begin
             ,r))))))

(define-syntax (rhombus-module-begin stx)
  (syntax-case stx ()
    [(_ (top . content))
     (unless (eq? 'top (syntax-e #'top))
       (raise-syntax-error #f "ill-formed body" stx))
     #`(#%module-begin
        (rhombus-top . content))]))
