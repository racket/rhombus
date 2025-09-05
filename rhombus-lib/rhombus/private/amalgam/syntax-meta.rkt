#lang racket/base
(require (for-syntax racket/base
                     racket/unsafe/undefined
                     racket/phase+space
                     racket/treelist
                     syntax/parse/pre
                     enforest/name-parse
                     shrubbery/print
                     "pack.rkt"
                     "dotted-sequence.rkt"
                     "define-arity.rkt"
                     "call-result-key.rkt"
                     "name-root.rkt"
                     (submod "annotation.rkt" for-class)
                     (only-in "static-info.rkt" #%none)
                     (for-syntax racket/base)
                     (submod "syntax-object.rkt" for-quasiquote)
                     "srcloc.rkt"
                     "context-stx.rkt"
                     "syntax-wrap.rkt"
                     "definition-context.rkt"
                     "class-primitive.rkt"
                     "name-equal.rkt")
         "space.rkt"
         "is-static.rkt"
         "operator-compare.rkt"
         "forwarding-sequence.rkt"
         "syntax-parameter.rkt"
         "parse.rkt")

(module+ for-unquote
  (provide (for-syntax syntax_meta.equal_binding)))

(begin-for-syntax
  (provide (for-space rhombus/namespace
                      syntax_meta)
           (for-space rhombus/annot
                      SyntaxPhase))

  (define-name-root syntax_meta
    #:fields
    ([equal_binding syntax_meta.equal_binding]
     [equal_name_and_scopes syntax_meta.equal_name_and_scopes]
     [binding_symbol syntax_meta.binding_symbol]
     [expanding_phase syntax_meta.expanding_phase]
     [error syntax_meta.error]
     [value syntax_meta.value]
     [flip_introduce syntax_meta.flip_introduce]
     [is_static syntax_meta.is_static]
     [dynamic_name syntax_meta.dynamic_name]
     [parse_dot_expr syntax_meta.parse_dot_expr]
     [parse_dot_repet syntax_meta.parse_dot_repet]
     [make_definition_context syntax_meta.make_definition_context]
     DefinitionContext))

  (define-primitive-class DefinitionContext definition-context
    #:lift-declaration
    #:existing
    #:just-annot
    #:fields
    ()
    #:properties
    ()
    #:methods
    ([add_definitions DefinitionContext.add_definitions]
     [add_scopes DefinitionContext.add_scopes]
     [call_using DefinitionContext.call_using]))

  (define expr-space-path (space-syntax #f))

  (define/arity (syntax_meta.value id/op
                                   [sp expr-space-path]
                                   [fail (lambda ()
                                           (raise-syntax-error who "no binding" id/op))])
    (define id (extract-name/sp who id/op sp))
    (syntax-local-value id (if (and (procedure? fail)
                                    (procedure-arity-includes? fail 0))
                               fail
                               (lambda () fail))))

  (define (extract-free-name who stx sp)
    (extract-name/sp who stx sp #:build-dotted? #t))

  (define/arity syntax_meta.equal_binding
    (case-lambda
      [(id1 id2)
       (free-identifier=? (extract-free-name who id1 expr-space-path)
                          (extract-free-name who id2 expr-space-path))]
      [(id1 id2 sp)
       (free-identifier=? (extract-free-name who id1 sp)
                          (extract-free-name who id2 sp))]
      [(id1 id2 sp phase1)
       (free-identifier=? (extract-free-name who id1 sp)
                          (extract-free-name who id2 sp)
                          phase1)]
      [(id1 id2 sp phase1 phase2)
       (free-identifier=? (extract-free-name who id1 sp)
                          (extract-free-name who id2 sp)
                          phase1
                          phase2)]))

  (define/arity (syntax_meta.equal_name_and_scopes id1
                                                   id2
                                                   [phase (syntax-local-phase-level)])
    (equal-name-and-scopes? who id1 id2 phase))

  (define/arity syntax_meta.binding_symbol
    (case-lambda
      [(id)
       (identifier-binding-symbol (extract-free-name who id expr-space-path))]
      [(id sp)
       (identifier-binding-symbol (extract-free-name who id sp))]
      [(id sp phase)
       (identifier-binding-symbol (extract-free-name who id sp)) phase]))

  (define (extract-name/sp who stx sp
                           #:build-dotted? [build-dotted? #f])
    (unless (space-name? sp) (raise-annotation-failure who sp "SpaceMeta"))
    (extract-name who stx (space-name-symbol sp)
                  #:build-dotted? build-dotted?))

  (define/arity (syntax_meta.expanding_phase)
    (syntax-local-phase-level))

  (define/arity (syntax_meta.error #:who [m-who #f]
                                   form/msg
                                   [form unsafe-undefined]
                                   [detail unsafe-undefined])
    #:static-infos ((#%call-result ((#%none #t))))
    (define who-in
      (cond
        [(or (not m-who) (symbol? m-who)) m-who]
        [(string? m-who) (string->symbol m-who)]
        [else
         (syntax-parse m-who
           #:datum-literals (group multi)
           [_::name #t]
           [(group _::dotted-operator-or-identifier-sequence) #t]
           [(multi (group _::dotted-operator-or-identifier-sequence)) #t]
           [_
            (raise-annotation-failure who m-who "error.Who")])
         (string->symbol (shrubbery-syntax->string #:use-raw? #t m-who))]))
    (cond
      [(eq? form unsafe-undefined)
       (define form form/msg)
       (unless (syntax*? form) (raise-annotation-failure who form "Syntax"))
       (raise-syntax-error who-in "bad syntax" (maybe-respan (syntax-unwrap form)))]
      [(eq? detail unsafe-undefined)
       (define msg form/msg)
       (unless (string? msg) (raise-annotation-failure who msg "ReadableString"))
       (unless (syntax*? form) (raise-annotation-failure who form "Syntax"))
       (raise-syntax-error who-in msg (maybe-respan (syntax-unwrap form)))]
      [else
       (define msg form/msg)
       (unless (string? msg) (raise-annotation-failure who msg "ReadableString"))
       (define (bad-detail)
         (raise-annotation-failure who detail "Syntax || List.of(Syntax)"))
       (define details (map maybe-respan (cond
                                           [(treelist? detail)
                                            (define l (treelist->list detail))
                                            (for ([i (in-list l)])
                                              (unless (syntax*? i) (bad-detail)))
                                            (map syntax-unwrap l)]
                                           [(syntax*? detail)
                                            (list (syntax-unwrap detail))]
                                           [else (bad-detail)])))
       (if (pair? details)
           (raise-syntax-error who-in msg
                               (maybe-respan form)
                               (car details)
                               (cdr details))
           (raise-syntax-error who-in msg
                               (maybe-respan form)))]))

  (define/arity (syntax_meta.flip_introduce stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (unless (syntax*? stx) (raise-annotation-failure who stx "Syntax"))
    (syntax-local-introduce (syntax-unwrap stx)))

  (define/arity (syntax_meta.is_static ctx-stx)
    (define ctx (extract-ctx who ctx-stx))
    (is-static-context? ctx))

  (define/arity (syntax_meta.dynamic_name name-stx
                                          #:as_static [static? #false]
                                          #:space [sp expr-space-path])
    (syntax-parse name-stx
      #:datum-literals (group multi)
      [n::name
       (unless (space-name? sp) (raise-annotation-failure who sp "SpaceMeta"))
       (relocate+reraw name-stx (add-dynamism-context #'n.name static? (space-name-symbol sp)))]
      [_
       (raise-annotation-failure who name-stx "Name")]))

  (define/arity (syntax_meta.make_definition_context [parent #f])
    #:static-infos ((#%call-result #,(get-definition-context-static-infos)))
    (unless (or (not parent) (definition-context? parent))
      (raise-annotation-failure who parent "DefinitionContext"))
    (definition-context
      (syntax-local-make-definition-context
       (and parent (definition-context-def-ctx parent)))
      (cons (gensym)
            (if parent
                (definition-context-expand-context parent)
                null))
      (box #hasheq())))

  (define/method (DefinitionContext.add_definitions ctx stx)
    (unless (definition-context? ctx)
      (raise-annotation-failure who ctx "DefinitionContext"))
    (unless (syntax? stx)
      (raise-annotation-failure who stx "Syntax"))
    (define gs (unpack-multi stx who #f))
    (expand-bridge-definition-sequence #`(rhombus-body-sequence #,@gs)
                                       (definition-context-def-ctx ctx)
                                       (definition-context-expand-context ctx)
                                       (definition-context-params-box ctx))
    (void))

  (define/method (DefinitionContext.add_scopes ctx stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (unless (definition-context? ctx)
      (raise-annotation-failure who ctx "DefinitionContext"))
    (unless (syntax? stx)
      (raise-annotation-failure who stx "Syntax"))
    (internal-definition-context-add-scopes
     (definition-context-def-ctx ctx)
     stx))

  (define/method (DefinitionContext.call_using ctx f)
    (unless (definition-context? ctx)
      (raise-annotation-failure who ctx "DefinitionContext"))
    (unless (and (procedure? f)
                 (procedure-arity-includes? f 0))
      (raise-annotation-failure who f "Function.of_arity(0)"))
    (with-continuation-mark
     syntax-parameters-key (unbox (definition-context-params-box ctx))
     (syntax-local-apply-transformer
      f
      #'cons
      (definition-context-expand-context ctx)
      (definition-context-def-ctx ctx))))

  (define-annotation-syntax SyntaxPhase
    (identifier-annotation phase? ())))
