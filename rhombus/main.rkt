#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     shrubbery/print)
         "private/bounce.rkt"
         "private/parse.rkt"
         "private/forwarding-sequence.rkt")

(provide (rename-out [rhombus-module-begin #%module-begin])
         #%top-interaction)

(bounce "private/implicit.rkt"
        "private/underscore.rkt"
        "private/arithmetic.rkt"
        "private/dot.rkt"
        "private/struct.rkt"
        "private/define.rkt"
        "private/value.rkt"
        "private/import.rkt"
        "private/export.rkt"
        "private/module-path.rkt"
        "private/operator.rkt"
        "private/annotation.rkt"
        "private/list.rkt"
        "private/array.rkt"
        "private/map.rkt"
        "private/assign.rkt"
        "private/function.rkt"
        "private/begin.rkt"
        "private/cond.rkt"
        "private/match.rkt"
        "private/quasiquote.rkt"
        "private/keyword.rkt"
        "private/values.rkt")

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
  (error-syntax->string-handler
   (lambda (s len)
     (shrubbery-syntax->string s #:max-length len)))
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
