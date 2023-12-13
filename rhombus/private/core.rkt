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
                              #%parens)
                     "syntax-parse-config.rkt")
         (only-in "declaration.rkt"
                  in-decl-space
                  decl-quote)
         racket/interaction-info
         "builtin-dot.rkt"
         "bounce.rkt"
         "parse.rkt"
         "forwarding-sequence.rkt"
         (submod "expression.rkt" for-top-expand)
         (only-in (submod "module.rkt" for-module-begin)
                  rhombus:module))

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
                     Identifier
                     Operator
                     Name
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
        "bits.rkt"
        "string.rkt"
        "bytes.rkt"
        "char.rkt"
        "symbol.rkt"
        "keyword.rkt"
        "dot.rkt"
        "dynamic-static.rkt"
        "def+let.rkt"
        "import.rkt"
        "export.rkt"
        "namespace.rkt"
        "module-path.rkt"
        "module-path-object.rkt"
        "operator.rkt"
        "annotation.rkt"
        "annotation-converting.rkt"
        "list.rkt"
        "pair.rkt"
        "array.rkt"
        "box.rkt"
        "map.rkt"
        "set.rkt"
        "indexable.rkt"
        "appendable.rkt"
        "assign.rkt"
        "equal.rkt"
        "function.rkt"
        "class.rkt"
        "class-clause-primitive.rkt"
        "with.rkt"
        "interface.rkt"
        "block.rkt"
        "cond.rkt"
        "match.rkt"
        "module.rkt"
        "quasiquote.rkt"
        "dollar.rkt"
        "unquote-binding-primitive.rkt"
        "rest-marker.rkt"
        "ellipsis.rkt"
        "apostrophe.rkt"
        "values.rkt"
        "print.rkt"
        "port.rkt"
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
        "parameter.rkt"
        "parameterize.rkt"
        "boolean-pattern.rkt"
        "boolean-annotation.rkt"
        "boolean-reducer.rkt"
        "equatable.rkt"
        "sequenceable.rkt"
        "eval.rkt"
        "printable.rkt"
        "callable.rkt"
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
      [(drracket:define-popup)
       (dynamic-require 'rhombus/private/define-popup
                        'define-popup)]
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
  (config-syntax-parse!)
  (check-unbound-identifier-early!)
  (syntax-parse stx
    [(_ (top . content))
     (unless (eq? 'top (syntax-e #'top))
       (raise-syntax-error #f "ill-formed body" stx))
     #`(#%module-begin
        (#%declare #:realm rhombus
                   #:require=define)
        (rhombus-forwarding-sequence
         #:module #f #f
         (rhombus-top . content))
        #,(let ([mode (ormap contigure-runtime-module-mode (syntax->list #'content))])
            (if mode
                #`(#,mode configure-runtime racket/base
                   (require (submod ".." configure_runtime)))
                #'(module configure-runtime racket/base
                    (require rhombus/runtime-config)))))]))

;; splices content of any block as its own top-level group:
(define-syntax (#%top-interaction stx)
  (config-syntax-parse!)
  (syntax-parse stx
    #:datum-literals (group block)
    [(form-id . (top form ... (group (block inner-form ...)) . content))
     #'(form-id . (top form ... inner-form ... . content))]
    [(_ . (top . content))
     #'(rhombus-top . content)]))

(define-for-syntax (contigure-runtime-module-mode g)
  (define (rhombus-mod? mod-id)
    (free-identifier=? (in-decl-space mod-id)
                       (decl-quote rhombus:module)))
  (syntax-parse g
    #:datum-literals (group parsed configure_runtime)
    #:literals (module module*)
    [(group mod #:early configure_runtime . _) #:when (rhombus-mod? #'mod) #'module]
    [(group mod #:late configure_runtime . _) #:when (rhombus-mod? #'mod) #'module*]
    [(group (parsed #:rhombus/decl (module configure_runtime . _))) #'module]
    [(group (parsed #:rhombus/decl (module* configure_runtime . _))) #'module*]
    [_ #f]))
