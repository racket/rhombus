#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     (only-in "ellipsis.rkt"
                              [... rhombus...])
                     (only-in "quasiquote.rkt" $)
                     "rest-marker.rkt")
         racket/interaction-info
         "builtin-dot.rkt"
         "bounce.rkt"
         "parse.rkt"
         "forwarding-sequence.rkt"
         (submod "expression.rkt" for-top-expand))

(provide (rename-out [rhombus-module-begin #%module-begin])
         #%top-interaction
         #%top
         (for-syntax
          (rename-out [rhombus... ...])
          $
          &
          ~&))

(bounce "default-stub.rkt"
        "implicit.rkt"
        "underscore.rkt"
        "arithmetic.rkt"
        "string.rkt"
        "dot.rkt"
        "maybe.rkt"
        "dynamic-static.rkt"
        "class.rkt"
        "interface.rkt"
        "class-together.rkt"
        "def+let.rkt"
        "import.rkt"
        "export.rkt"
        "namespace.rkt"
        "module-path.rkt"
        "operator.rkt"
        "annotation.rkt"
        "list.rkt"
        "pair.rkt"
        "array.rkt"
        "map.rkt"
        "set.rkt"
        "map-ref.rkt"
        "assign.rkt"
        "equal.rkt"
        "function.rkt"
        "rule.rkt"
        "begin.rkt"
        "cond.rkt"
        "match.rkt"
        "quasiquote.rkt"
        "unquote-binding-primitive.rkt"
        "rest-marker.rkt"
        "ellipsis.rkt"
        "apostrophe.rkt"
        "values.rkt"
        "print.rkt"
        "syntax-object.rkt"
        "syntax-class.rkt"
        "syntax-class-syntax.rkt"
        "syntax-class-clause-primitive.rkt"
        "pattern.rkt"
        "pattern-clause-primitive.rkt"
        "for.rkt"
        "range.rkt"
        "parameterize.rkt"
        "boolean-pattern.rkt"
        "boolean-annotation.rkt"
        "equatable.rkt"
        "eval.rkt"
        "printable.rkt"
        "path-object.rkt"
        "srcloc-object.rkt")

(module reader syntax/module-reader
  #:language 'rhombus/private/core
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
