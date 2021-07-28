#lang racket/base
(require (for-syntax racket/base)
         "parse.rkt"
         "private/declaration-sequence.rkt")

(provide (rename-out [rhombus-module-begin #%module-begin]))

(define-syntax (bounce stx)
  (syntax-case stx ()
    [(_ mod ...)
     (with-syntax ([(mod ...) ((make-syntax-introducer) #'(mod ...))])
       #'(begin (begin (require mod (for-syntax mod))
                       (provide (all-from-out mod)
                                (for-syntax (all-from-out mod))))
                ...))]))
(bounce "private/core-implicit.rkt"
        "private/core-op.rkt"
        "private/struct.rkt"
        "private/define.rkt"
        "private/require.rkt"
        "private/provide.rkt"
        "private/expression-syntax.rkt"
        "private/binding-syntax.rkt"
        "private/definition-syntax.rkt"
        "private/declaration-syntax.rkt"
        "private/type.rkt"
        "private/list.rkt"
        "private/assign.rkt"
        "private/function.rkt"
        "private/cond.rkt"
        "private/match.rkt"
        "private/quasiquote.rkt"
        "private/values.rkt")

(module reader syntax/module-reader
  #:language 'rhombus
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (only-in (submod shrubbery reader)
                    get-info-proc)))

(define-syntax (rhombus-module-begin stx)
  (syntax-case stx ()
    [(_ (top . content))
     (unless (eq? 'top (syntax-e #'top))
       (raise-syntax-error #f "ill-formed body" stx))
     #`(#%module-begin
        (rhombus-declaration-sequence
         (rhombus-top . content)))]))
