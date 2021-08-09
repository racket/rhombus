#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "private/parse.rkt"
         "private/forwarding-sequence.rkt")

(provide (rename-out [rhombus-module-begin #%module-begin])
         #%top-interaction)

(define-syntax (bounce stx)
  (syntax-case stx ()
    [(_ mod ...)
     (with-syntax ([(mod ...) ((make-syntax-introducer) #'(mod ...))])
       #'(begin (begin (require mod (for-syntax mod))
                       (provide (all-from-out mod)
                                (for-syntax (all-from-out mod))))
                ...))]))
(bounce "private/implicit.rkt"
        "private/arithmetic.rkt"
        "private/dot.rkt"
        "private/struct.rkt"
        "private/define.rkt"
        "private/value.rkt"
        "private/import.rkt"
        "private/export.rkt"
        "private/module-path.rkt"
        "private/expression-syntax.rkt"
        "private/binding-syntax.rkt"
        "private/definition-syntax.rkt"
        "private/declaration-syntax.rkt"
        "private/operator.rkt"
        "private/contract.rkt"
        "private/list.rkt"
        "private/array.rkt"
        "private/map.rkt"
        "private/assign.rkt"
        "private/function.rkt"
        "private/cond.rkt"
        "private/match.rkt"
        "private/quasiquote.rkt"
        "private/values.rkt"
        "private/contract-syntax.rkt"
        "private/static-info-syntax.rkt"
        "private/dot-syntax.rkt")

(module reader syntax/module-reader
  #:language 'rhombus
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info (lambda (key default make-default)
           (case key
             [(drracket:submit-predicate)
              (lambda (in whitespace-after?)
                (and whitespace-after?
                     (regexp-match? #px"(?m:^);$" in)))]
             [else (get-info-proc key default make-default)]))
  #:whole-body-readers? #t
  (require shrubbery/parse
           (only-in (submod shrubbery reader)
                    get-info-proc)))

(module configure-runtime racket/base
  (require rhombus/runtime-config))

(define-syntax (rhombus-module-begin stx)
  (syntax-parse stx
    [(_ (top . content))
     (unless (eq? 'top (syntax-e #'top))
       (raise-syntax-error #f "ill-formed body" stx))
     #`(#%module-begin
        (module configure-runtime racket/base (require rhombus/runtime-config))
        (rhombus-forwarding-sequence
         (rhombus-top . content)))]))

(define-syntax (#%top-interaction stx)
  (syntax-parse stx
    [(_ . (top . content))
     #'(rhombus-top . content)]))
