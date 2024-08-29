#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
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
         (only-in "declaration.rkt"
                  in-decl-space
                  decl-quote)
         "builtin-dot.rkt"
         "../bounce.rkt"
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
        "byte.rkt"
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
        "key-comp-primitive.rkt"
        "indexable.rkt"
        "appendable.rkt"
        "comparable.rkt"
        "listable.rkt"
        "assign.rkt"
        "equal.rkt"
        "function.rkt"
        "class.rkt"
        "class-clause-primitive.rkt"
        "with.rkt"
        "interface.rkt"
        "veneer.rkt"
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
        "for-clause-primitive.rkt"
        "range.rkt"
        "control.rkt"
        "exn-object.rkt"
        "parameter.rkt"
        "parameterize.rkt"
        "boolean-pattern.rkt"
        "boolean-annotation.rkt"
        "boolean-reducer.rkt"
        "boolean-repet.rkt"
        "arrow-annotation.rkt"
        "equatable.rkt"
        "sequenceable.rkt"
        "eval.rkt"
        "printable.rkt"
        "callable.rkt"
        "path-object.rkt"
        "srcloc-object.rkt")

(module reader racket/base
  (require (submod rhombus/private/core reader))
  (provide (all-from-out (submod rhombus/private/core reader))))

(module configure-runtime racket/base
  (require rhombus/private/amalgam/runtime-config)
  (void (install-runtime-config!)))

(module configure-expand racket/base
  (require rhombus/private/amalgam/expand-config)
  (provide enter-parameterization
           exit-parameterization))

(module+ module-begin
  (provide (for-syntax check-unbound-identifier-early!)))

(define-syntax (rhombus-module-begin stx)
  (do-rhombus-module-begin stx #'(require rhombus/runtime-config)))

(define-syntax (rhombus-amalgam-module-begin stx)
  (do-rhombus-module-begin stx #'(begin)))
(module+ amalgam-module-begin
  (provide (rename-out [rhombus-amalgam-module-begin
                        #%module-begin])))

(define-for-syntax (do-rhombus-module-begin stx configure-runtime-body)
  (check-unbound-identifier-early!)
  (syntax-parse stx
    [(_ body)
     #:with content (syntax-parse #'body
                      #:datum-literals (multi group)
                      [(multi . content) #'content]
                      [(group . _) (list #'body)]
                      [else
                       (raise-syntax-error #f "ill-formed body" stx)])
     #`(#%module-begin
        (#%declare #:realm rhombus
                   #:require=define)
        (rhombus-module-forwarding-sequence
         (rhombus-top . content))
        #,(let ([mode (ormap contigure-runtime-module-mode (syntax->list #'content))])
            (if mode
                #`(#,mode configure-runtime racket/base
                   (require (submod ".." configure_runtime)))
                #`(module configure-runtime racket/base
                    #,configure-runtime-body))))]))

;; splices content of any block as its own top-level group:
(define-syntax (#%top-interaction stx)
  (syntax-parse stx
    #:datum-literals (group block multi)
    [(form-id . (multi form ... (group (block inner-form ...)) . content))
     #'(form-id . (multi form ... inner-form ... . content))]
    [(_ . (multi . content))
     #'(rhombus-top . content)]
    [(form-id . (~and g (group . _)))
     #'(form_id . (multi g))]))

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
