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
                     "srcloc.rkt")
         (only-in "declaration.rkt"
                  in-decl-space
                  decl-quote
                  define-decl-syntax
                  declaration-transformer)
         (only-in "definition.rkt"
                  in-defn-space)
         "builtin-dot.rkt"
         "../bounce.rkt"
         "parse.rkt"
         "forwarding-sequence.rkt"
         "parens.rkt"
         "effect.rkt"
         (submod "expression.rkt" for-top-expand)
         (only-in (submod "module.rkt" for-module-begin)
                  rhombus:module))

(provide (rename-out [rhombus-module-begin #%module-begin])
         #%top-interaction
         #%top
         (for-space rhombus/decl
                    #%module_block
                    module_block
                    #%interaction)
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
        "order-primitive.rkt"
        "annotation.rkt"
        "annotation-converting.rkt"
        "list.rkt"
        "pair.rkt"
        "array.rkt"
        "box.rkt"
        "map.rkt"
        "set.rkt"
        "key-comp-primitive.rkt"
        "map-maybe.rkt"
        "indexable.rkt"
        "appendable.rkt"
        "comparable.rkt"
        "listable.rkt"
        "membership-testable.rkt"
        "not-infix.rkt"
        "assign.rkt"
        "equal.rkt"
        "repet-primitive.rkt"
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
        "match-ns.rkt"
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

(module+ module-block
  (provide (for-space rhombus/decl
                      module_block_no_submodules)))

(define-syntax (rhombus-module-begin stx)
  (do-rhombus-module-begin stx #t))

(define-syntax (rhombus-amalgam-module-begin stx)
  (do-rhombus-module-begin stx #f))
(module+ amalgam-module-begin
  (provide (rename-out [rhombus-amalgam-module-begin
                        #%module-begin])))

(define-for-syntax (do-rhombus-module-begin stx use-module-block?)
  (check-unbound-identifier-early!)
  (syntax-parse stx
    [(_ body)
     #:with content (syntax-parse #'body
                      #:datum-literals (multi group)
                      [(multi . content) #'content]
                      [(group . _) (list #'body)]
                      [else
                       (raise-syntax-error #f "ill-formed body" stx)])
     (with-syntax ([#%module_block (datum->syntax stx '#%module_block)])
       (unless (declaration-bound? #'#%module_block)
         (raise-no-module-block stx #'content))
       #`(#%plain-module-begin
          (#%declare #:realm rhombus
                     #:require=define)
          #,(if use-module-block?
                #`(rhombus-top
                   (group #%module_block (block . content)))
                #`(rhombus-top . content))))]))

(define-decl-syntax #%module_block
  (declaration-transformer
   (lambda (stx)
     (parse-module-block stx #t #'#%effect))))

(define-decl-syntax module_block
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (b-tag::block
                  (~alt (~optional (group #:effect ~! (~or (_::block (group effect-id:identifier))
                                                           effect-id:identifier)))
                        (~optional (group (~and #:no_additional_submodules no-submod) ~!)))
                  ...
                  . content))
        (parse-module-block #'(form-id (b-tag . content))
                            (not (attribute no-submod))
                            (or (attribute effect-id)
                                #'#%effect))]))))

(define-decl-syntax module_block_no_submodules
  (declaration-transformer
   (lambda (stx)
     (parse-module-block stx #f #f))))

(define-for-syntax (parse-module-block stx submodules? effect-id)
  (syntax-parse stx
    #:datum-literals ()
    [(form-id (_::block . content))
     (define contents (syntax->list #'content))
     #`((rhombus-module-forwarding-sequence
         (rhombus-module-top #,effect-id . content))
        #,@(cond
             [submodules?
              (cons
               (let ([mode (ormap (configure-module-mode 'configure_runtime) contents)])
                 (if mode
                     #`(#,mode configure-runtime racket/base
                        (require (submod ".." configure_runtime)))
                     #`(module configure-runtime racket/base
                         (require rhombus/runtime-config))))
               (cond
                 [(ormap (configure-module-mode 'configure_expand) contents)
                  => (lambda (mode)
                       #`((#,mode configure-expand racket/base
                           (require (only-in (submod ".." configure_expand)
                                             enter_parameterization
                                             exit_parameterization))
                           (provide (rename-out [enter_parameterization enter-parameterization]
                                                [exit_parameterization exit-parameterization])))))]
                 [(ormap (configure-module-mode 'reader) contents)
                  ;; `reader` but no `configure_expand` => add Rhombusy `configure-expand`
                  #`((module configure-expand racket/base
                       (require rhombus/expand-config)
                       (provide enter-parameterization
                                exit-parameterization)))]
                 [else
                  null]))]
             [else null]))]))

;; splices content of any block as its own top-level group:
(define-syntax (#%top-interaction stx)
  (with-syntax ([#%interaction (datum->syntax stx '#%interaction)])
    (syntax-parse stx
      #:datum-literals (group multi)
      [(_ . (multi . content))
       (unless (declaration-bound? #'#%interaction)
         (raise-no-interaction stx #'content))
       #'(rhombus-top
          (group #%interaction (block . content)))]
      [(form-id . (~and g (group . _)))
       (unless (declaration-bound? #'#%interaction)
         (raise-no-interaction stx #'(g)))
       #'(rhombus-top
          (group #%interaction (block g)))])))

(define-decl-syntax #%interaction
  (declaration-transformer
   (lambda (stx)
     (let loop ([stx stx])
       (syntax-parse stx
         [(form-id (tag::block form ... (group (_::block inner-form ...)) . content))
          (loop #'(form-id (tag form ... inner-form ... . content)))]
         [(_ (_::block . content))
          #'((rhombus-top . content))])))))

(define-for-syntax ((configure-module-mode name) g)
  (define (hit? id)
    (eq? (syntax-e id) name))
  (define (rhombus-mod? mod-id)
    (free-identifier=? (in-decl-space mod-id)
                       (decl-quote rhombus:module)))
  (syntax-parse g
    #:datum-literals (group parsed)
    #:literals (module module*)
    [(group mod #:early name . _) #:when (and (hit? #'name) (rhombus-mod? #'mod)) #'module]
    [(group mod #:late name . _) #:when (and  (hit? #'name) (rhombus-mod? #'mod)) #'module*]
    [(group mod #:splice name . _) #:when (and  (hit? #'name) (rhombus-mod? #'mod)) #'module*]
    [(group mod name #:lang . _) #:when (and  (hit? #'name) (rhombus-mod? #'mod)) #'module]
    [(group (parsed #:rhombus/decl (module name . _))) #:when (hit? #'name) #'module]
    [(group (parsed #:rhombus/decl (module* name . _))) #:when (hit? #'name) #'module*]
    [_ #f]))

(define-for-syntax (declaration-bound? id)
  (or (identifier-binding id)
      (identifier-binding (in-decl-space id))
      (identifier-binding (in-defn-space id))))

(define-for-syntax (raise-no-module-block stx content)
  ;; Since the module didn't export `#%module_block`, it probably also didn't
  ;; have a `configure_expand` submodule, so manually configure syntax reporting:
  (parameterize ([error-syntax->string-handler
                  (lambda (stx len)
                    (define str (shrubbery-syntax->string stx #:max-length len))
                    (if (equal? str "")
                        "[end of group]"
                        str))])
    (raise-syntax-error 'module
                        "no `#%module_block` from the module used as a language"
                        (if (null? (syntax-e content))
                            (datum->syntax #f '(multi) stx)
                            (respan
                             (no-srcloc #`(multi . #,content)))))))

(define-for-syntax (raise-no-interaction stx content)
  (raise-syntax-error 'module
                      "no `#%interaction` available for interactive evaluation"
                      (if (null? (syntax-e content))
                          (datum->syntax #f '(multi) stx)
                          (respan (no-srcloc #`(multi . #,content))))))
