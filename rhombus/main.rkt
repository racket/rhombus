#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     shrubbery/print
                     (only-in "private/ellipsis.rkt"
                              [... rhombus...])
                     (only-in "private/quasiquote.rkt" $)
                     "private/rest-marker.rkt")
         racket/interaction-info
         "private/builtin-dot.rkt"
         "private/bounce.rkt"
         "private/parse.rkt"
         "private/forwarding-sequence.rkt"
         (submod "private/expression.rkt" for-top-expand))

(provide (rename-out [rhombus-module-begin #%module-begin])
         #%top-interaction
         (for-syntax
          (rename-out [rhombus... ...])
          $
          &
          ~&))

(bounce "private/default-stub.rkt"
        "private/implicit.rkt"
        "private/underscore.rkt"
        "private/arithmetic.rkt"
        "private/string.rkt"
        "private/dot.rkt"
        "private/dynamic-static.rkt"
        "private/class.rkt"
        "private/interface.rkt"
        "private/def+let.rkt"
        "private/import.rkt"
        "private/export.rkt"
        "private/namespace.rkt"
        "private/module-path.rkt"
        "private/operator.rkt"
        "private/annotation.rkt"
        "private/list.rkt"
        "private/pair.rkt"
        "private/array.rkt"
        "private/map.rkt"
        "private/set.rkt"
        "private/map-ref.rkt"
        "private/assign.rkt"
        "private/equal.rkt"
        "private/function.rkt"
        "private/rule.rkt"
        "private/begin.rkt"
        "private/cond.rkt"
        "private/match.rkt"
        "private/quasiquote.rkt"
        "private/rest-marker.rkt"
        "private/ellipsis.rkt"
        "private/keyword.rkt"
        "private/symbol.rkt"
        "private/values.rkt"
        "private/print.rkt"
        "private/syntax-object.rkt"
        "private/syntax-class.rkt"
        "private/syntax-class-syntax.rkt"
        "private/for.rkt"
        "private/range.rkt"
        "private/parameterize.rkt"
        "private/boolean-pattern.rkt"
        "private/boolean-annotation.rkt"
        "private/eval.rkt"
        "private/printer.rkt")

(module reader syntax/module-reader
  #:language 'rhombus
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (only-in (submod shrubbery reader)
                    [get-info-proc shrubbery:get-info-proc]))
  (provide get-info-proc)
  (define (get-info-proc key default make-default)
    (case key
      [(drracket:default-extension) "rhm"]
      [else (shrubbery:get-info-proc key default make-default)])))

(module configure-runtime racket/base
  (require rhombus/runtime-config))

(define-syntax (rhombus-module-begin stx)
  (error-syntax->string-handler
   (lambda (s len)
     (shrubbery-syntax->string s #:max-length len)))
  (check-unbound-identifier-early!)
  (syntax-parse stx
    [(_ (top . content))
     (unless (eq? 'top (syntax-e #'top))
       (raise-syntax-error #f "ill-formed body" stx))
     #`(#%module-begin
        (module configure-runtime racket/base (require rhombus/runtime-config))
        (#%declare #:realm rhombus
                   #:require=define)
        (rhombus-forwarding-sequence
         #:module #f #f
         (rhombus-top . content)))]))

;; splices content of any block as its own top-level group:
(define-syntax (#%top-interaction stx)
  (syntax-parse stx
    #:datum-literals (group block)
    [(form-id . (top form ... (group (block inner-form ...)) . content))
     #'(form-id . (top form ... inner-form ... . content))]
    [(_ . (top . content))
     #'(rhombus-top . content)]))
