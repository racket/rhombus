#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     (only-in "ellipsis.rkt"
                              [... rhombus...])
                     (only-in "quasiquote.rkt"
                              [_ rhombus_]
                              #%quotes)
                     (only-in "unquote-binding-primitive.rkt"
                              #%parens
                              ::)
                     (only-in "dollar.rkt" $)
                     (only-in "match.rkt" match)
                     "rest-marker.rkt"
                     (only-space-in rhombus/stxclass
                                    "syntax-class-primitive.rkt")
                     (only-in "implicit.rkt"
                              #%parens))
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
          ;; anything exported in the default space needs
          ;; to be exported in all of its spaces
          (for-space #f
                     (rename-out [rhombus... ...])
                     $
                     &
                     ~&
                     #%quotes
                     #%parens
                     match)
          (for-space rhombus/bind
                     (rename-out [rhombus... ...])
                     $
                     &
                     ~&
                     #%quotes
                     #%parens)
          (for-space rhombus/unquote_bind
                     (rename-out [rhombus_ _])
                     #%quotes
                     #%parens
                     ::)
          (for-space rhombus/repet
                     #%quotes
                     #%parens)
          (for-space rhombus/stxclass
                     ;; Why doesn't
                     ;;   (all-from-out "syntax-class-primitive.rkt")
                     ;; work here?
                     Term
                     Id
                     Op
                     Id_Op
                     Group
                     Block
                     Multi
                     Keyword
                     String
                     Int)))

(bounce "implicit.rkt"
        "underscore.rkt"
        "arithmetic.rkt"
        "math.rkt"
        "string.rkt"
        "bytes.rkt"
        "dot.rkt"
        "maybe.rkt"
        "dynamic-static.rkt"
        "class.rkt"
        "class-clause-primitive.rkt"
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
        "macro.rkt"
        "begin.rkt"
        "cond.rkt"
        "match.rkt"
        "submodule.rkt"
        "quasiquote.rkt"
        "dollar.rkt"
        "unquote-binding-primitive.rkt"
        "rest-marker.rkt"
        "ellipsis.rkt"
        "apostrophe.rkt"
        "values.rkt"
        "print.rkt"
        "syntax-object.rkt"
        "syntax-class-primitive.rkt"
        "syntax-class.rkt"
        "syntax-class-clause-primitive.rkt"
        "pattern.rkt"
        "pattern-clause-primitive.rkt"
        "for.rkt"
        "range.rkt"
        "control.rkt"
        "exn-object.rkt"
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

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))

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
