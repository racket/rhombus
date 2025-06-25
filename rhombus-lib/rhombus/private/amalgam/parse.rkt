#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/implicit
                     shrubbery/property
                     "srcloc.rkt")
         "enforest.rkt"
         "name-root-ref.rkt"
         "forwarding-sequence.rkt"
         "declaration.rkt"
         "nestable-declaration.rkt"
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "parens.rkt"
         "static-info.rkt")

(provide rhombus-top
         rhombus-nested
         rhombus-definition
         rhombus-body
         rhombus-expression

         rhombus-body-at
         rhombus-body-expression
         rhombus-body-sequence

         rhombus-top-step

         maybe-definition

         (for-syntax :declaration
                     :nestable-declaration
                     :definition
                     :expression
                     :binding

                     declaration?
                     nestable-declaration?
                     definition?
                     definition-sequence?

                     ;; for continuing enforestation of expressions or bindings:
                     :prefix-op+expression+tail
                     :infix-op+expression+tail
                     :prefix-op+binding+tail
                     :infix-op+binding+tail

                     enforest-expression-block
                     expression-relative-precedence
                     binding-relative-precedence

                     rhombus-local-expand))

(begin-for-syntax
  ;; treating `:` as infix improves error messages in some spaces
  (define (select-infix-implicit/block head)
    (syntax-parse head
      [((~and tag (~datum block)) . _)
       (values '#%block #'tag)]
      [_ (select-infix-implicit head)]))

  ;; Form at the top of a module:
  (define-rhombus-transform
    #:syntax-class :declaration
    #:predicate declaration?
    #:desc "declaration"
    #:parsed-tag #:rhombus/decl
    #:in-space in-decl-space
    #:transformer-ref declaration-transformer-ref
    #:check-result check-declaration-result
    #:track-origin track-sequence-origin
    #:use-site-scopes? #t)

  ;; Form at the top of a module or in a `nested` block:
  (define-rhombus-transform
    #:syntax-class (:nestable-declaration name-prefix)
    #:predicate nestable-declaration?
    #:desc "nestable declaration"
    #:parsed-tag #:rhombus/decl/nestable
    #:in-space in-decl-space
    #:transformer-ref nestable-declaration-transformer-ref
    #:check-result check-nestable-declaration-result
    #:track-origin track-sequence-origin
    #:use-site-scopes? #t)

  ;; Form in a definition context:
  (define-rhombus-transform
    #:syntax-class (:definition name-prefix)
    #:predicate definition?
    #:desc "definition"
    #:parsed-tag #:rhombus/defn
    #:in-space in-defn-space
    #:transformer-ref definition-transformer-ref
    #:check-result check-definition-result
    #:track-origin track-sequence-origin
    #:use-site-scopes? #t)

  ;; Form in a definition context that can consume extra groups:
  (define-rhombus-sequence-transform
    #:syntax-class :definition-sequence
    #:apply-transformer apply-definition-sequence-transformer
    #:predicate definition-sequence?
    #:desc "definition sequence"
    #:in-space in-defn-space
    #:transformer-ref definition-sequence-transformer-ref
    #:check-result check-definition-result
    #:track-origin track-sequence-origin
    #:use-site-scopes? #t)

  ;; Form in an expression context:
  (define-rhombus-enforest
    #:syntax-class :expression
    #:prefix-more-syntax-class :prefix-op+expression+tail
    #:infix-more-syntax-class :infix-op+expression+tail
    #:desc "expression"
    #:parsed-tag #:rhombus/expr
    #:operator-desc "expression operator"
    #:in-space in-expression-space
    #:prefix-operator-ref expression-prefix-operator-ref
    #:infix-operator-ref expression-infix-operator-ref
    #:select-infix-implicit select-infix-implicit/block
    #:check-result check-expression-result
    #:make-identifier-form make-identifier-expression
    #:relative-precedence expression-relative-precedence
    #:use-site-scopes? #t)

  (define name-root-binding-ref
    (make-name-root-ref #:binding-ref (lambda (v)
                                        (or (binding-prefix-operator-ref v)
                                            (binding-infix-operator-ref v)))
                        #:binding-extension-combine binding-extension-combine))

  ;; Form in a binding context:
  (define-rhombus-enforest
    #:syntax-class :binding
    #:prefix-more-syntax-class :prefix-op+binding+tail
    #:infix-more-syntax-class :infix-op+binding+tail
    #:desc "binding"
    #:operator-desc "binding operator"
    #:parsed-tag #:rhombus/bind
    #:in-space in-binding-space
    #:prefix-operator-ref binding-prefix-operator-ref
    #:infix-operator-ref binding-infix-operator-ref
    #:select-infix-implicit select-infix-implicit/block
    #:check-result check-binding-result
    #:make-identifier-form make-identifier-binding
    #:relative-precedence binding-relative-precedence
    #:name-root-ref name-root-binding-ref))

;; For a module top level, interleaves expansion and enforestation:
(define-syntax (rhombus-top stx)
  (syntax-parse stx
    [(_ . rest) #'(rhombus-top-step rhombus-top #t #f () . rest)]))

;; For a nested context
(define-syntax (rhombus-nested stx)
  (syntax-parse stx
    [(_ prefix . rest) #'(rhombus-top-step rhombus-nested #f prefix (prefix) . rest)]))

;; Trampoline variant where `top` for return is provided first
(define-syntax (rhombus-top-step stx)
  (with-syntax-error-respan
    (syntax-parse stx
      #:datum-literals (group parsed)
      [(_ top decl-ok? prefix data) #`(begin)]
      [(_ top decl-ok? prefix (data ...) (group (parsed #:rhombus/decl decl)) . forms)
       #`(begin decl (top data ... . forms))]
      ;; note that we may perform hierarchical name resolution
      ;; up to four times, since resolution in `:declaration`,
      ;; `:definition`, etc., doesn't carry over
      [(_ top decl-ok? prefix (data ...) e::definition-sequence . tail)
       (define-values (parsed new-tail)
         (apply-definition-sequence-transformer #'e #'e.id #'e.tail #'tail (and (syntax-e #'prefix) #'prefix)))
       #`(begin (begin . #,parsed) (top data ... . #,new-tail))]
      [(_ top decl-ok? prefix (data ...) form . forms)
       (define d-prefix (and (syntax-e #'prefix) #'prefix))
       (define (nestable-parsed)
         (syntax-parse #'form
           [(~var e (:nestable-declaration d-prefix)) #'(begin . e.parsed)]
           [(~var e (:definition d-prefix)) #'(begin . e.parsed)]
           [_ (relocate (maybe-respan #'form)
                        #`(#%expression (rhombus-expression form)))]))
       (define parsed
         (if (syntax-e #'decl-ok?)
             (syntax-parse #'form
               [e::declaration #'(begin . e.parsed)]
               [_ (nestable-parsed)])
             (nestable-parsed)))
       (syntax-parse #'forms
         [() parsed]
         [_ #`(begin #,parsed (top data ... . forms))])])))

;; For a definition context:
(define-syntax (rhombus-definition stx)
  (with-syntax-error-respan
    (syntax-parse stx
      #:datum-literals (group parsed)
      [(_) #'(begin)]
      [(_ (group (parsed #:rhombus/defn defn))) #'defn]
      [(_ (~var e (:definition #f))) #'(begin . e.parsed)]
      [(_ form) (relocate (maybe-respan #'form)
                          #'(#%expression (rhombus-expression form)))])))

;; For an expression context, interleaves expansion and enforestation:
(define-syntax (rhombus-body stx)
  (syntax-parse stx
    [(_)
     (raise-syntax-error #f "block has no expressions" stx)]
    [(_ g) ; try shortcut for a single expression
     #:when (not (definition? #'g))
     #`(rhombus-expression g)]
    [(_ . tail)
     #`(let ()
         (rhombus-block-forwarding-sequence
          #:orig #,stx
          (rhombus-body-sequence . tail)))]))

;; Like `rhombus-body`, but potentially improves the error message
(define-syntax (rhombus-body-at stx)
  (syntax-parse stx
    [(_ tag)
     (raise-syntax-error #f "block has no expressions" #'tag)]
    [(_ tag . tail)
     (datum->syntax #f (cons #'rhombus-body #'tail))]))

;; Like `(rhombus-expression (group _))`, but recognizes `block` forms
(define-syntax (rhombus-body-expression stx)
  (syntax-parse stx
    #:datum-literals (parsed)
    [(_ (body-tag::block body ...))
     #'(rhombus-body-at body-tag body ...)]
    [(_ (parsed #:rhombus/expr rhs))
     #'rhs]
    [(_ rhs)
     (datum->syntax #f (list #'rhombus-expression #'rhs))]))

;; For a definition context, interleaves expansion and enforestation:
(define-syntax (rhombus-body-sequence stx)
  (with-syntax-error-respan
    (syntax-parse stx
      #:datum-literals (group parsed)
      #:literals (maybe-definition)
      [(_) #'(begin)]
      [(_ (group (parsed #:rhombus/expr (maybe-definition e))) . tail)
       #`(begin e . tail)]
      [(_ e::definition-sequence . tail)
       (define-values (parsed new-tail)
         (apply-definition-sequence-transformer #'e #'e.id #'e.tail #'tail #f))
       #`(begin
           (begin . #,parsed)
           (rhombus-body-sequence . #,new-tail))]
      [(_ (~var e (:definition #f)) . tail)
       #`(begin
           (begin . e.parsed)
           (rhombus-body-sequence . tail))]
      [(_ g . tail)
       #`(begin
           #,(relocate (maybe-respan #'g) #`(#%expression (rhombus-expression g)))
           (rhombus-body-sequence . tail))])))

;; Wraps a `parsed` term that can be treated as a definition:
(define-syntax (maybe-definition stx)
  (syntax-parse stx
    [(_ e) #'e]))

;; For an expression context:
(define-syntax (rhombus-expression stx)
  ;; The `enforest-rhombus-expression` function expects to be called
  ;; during the dynamic extent of a Rhombus transformer, so we
  ;; add calls to `syntax-local-introduce` to cancel the ones in
  ;; `enforest-rhombus-expression`. Also, if we get here, then
  ;; we're expanding a Rhombus expression in a Racket context, and
  ;; the Racket context isn't going to look at static info, so we
  ;; can prune it.
  (define new-stx (discard-static-infos
                   (syntax-local-introduce
                    (enforest-rhombus-expression (syntax-local-introduce stx)))))
  ;; We don't want an 'opaque-raw property to be duplicated. So,
  ;; if it exists on the input, discard any such property on the
  ;; output.
  (if (syntax-opaque-raw-property stx)
      (let* ([new-stx (syntax-opaque-raw-property new-stx "")]
             [new-stx (syntax-raw-suffix-property new-stx '())]
             [new-stx (syntax-raw-prefix-property new-stx '())])
        new-stx)
      new-stx))

(define-for-syntax (enforest-rhombus-expression stx)
  (with-syntax-error-respan
    (syntax-parse (syntax-local-introduce stx)
      [(_ e::expression) (syntax-local-introduce #'e.parsed)])))

;; If `e` is a block with a single group, and if the group is not a
;; definition, then parse the expression
(define-for-syntax (enforest-expression-block e)
  (syntax-parse e
    #:datum-literals (group)
    [(tag::block g)
     #:when (not (definition? #'g))
     #:with g-e::expression #'g
     #'g-e.parsed]
    [(tag::block g ...) #'(rhombus-body-at tag g ...)]
    [(~and (group . _) g-e::expression) #'g-e.parsed]))

;; Forces enforestation through `rhombus-expression`; note that
;; `#%parens` eagerly enforests its content, so this effectively
;; goes eagerly through parentheses
(define-for-syntax (rhombus-local-expand stx)
  (syntax-parse stx
    #:literals (rhombus-expression)
    [(rhombus-expression . _)
     (rhombus-local-expand (enforest-rhombus-expression stx))]
    [_ stx]))
