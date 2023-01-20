#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx
                     enforest
                     enforest/transformer
                     "srcloc.rkt"
                     "name-path-op.rkt")
         "enforest.rkt"
         "name-root-ref.rkt"
         "forwarding-sequence.rkt"
         "declaration.rkt"
         "nestable-declaration.rkt"
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "srcloc.rkt")

(provide rhombus-top
         rhombus-nested
         rhombus-definition
         rhombus-body
         rhombus-expression

         rhombus-body-at
         rhombus-body-expression
         rhombus-body-sequence

         rhombus-top-step

         (for-syntax :declaration
                     :nestable-declaration
                     :definition
                     :expression
                     :binding

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
  ;; Form at the top of a module:
  (define-rhombus-transform
    #:syntax-class :declaration
    #:desc "declaration"
    #:in-space in-expression-space
    #:transformer-ref declaration-transformer-ref
    #:check-result check-declaration-result)

  ;; Form at the top of a module or in a `nested` block:
  (define-rhombus-transform
    #:syntax-class :nestable-declaration
    #:desc "nestable declaration"
    #:in-space in-expression-space
    #:transformer-ref nestable-declaration-transformer-ref
    #:check-result check-nestable-declaration-result)

  ;; Form in a definition context:
  (define-rhombus-transform
    #:syntax-class :definition
    #:predicate definition?
    #:desc "definition"
    #:in-space in-expression-space
    #:transformer-ref definition-transformer-ref
    #:check-result check-definition-result)

  ;; Form in a definition context that can consume extra groups:
  (define-rhombus-sequence-transform
    #:syntax-class :definition-sequence
    #:apply-transformer apply-definition-sequence-transformer
    #:predicate definition-sequence?
    #:desc "definition sequence"
    #:in-space in-expression-space
    #:transformer-ref definition-sequence-transformer-ref
    #:check-result check-definition-result)

  ;; Form in an expression context:
  (define-rhombus-enforest
    #:syntax-class :expression
    #:prefix-more-syntax-class :prefix-op+expression+tail
    #:infix-more-syntax-class :infix-op+expression+tail
    #:desc "expression"
    #:operator-desc "expression operator"
    #:in-space in-expression-space
    #:prefix-operator-ref expression-prefix-operator-ref
    #:infix-operator-ref expression-infix-operator-ref
    #:check-result check-expression-result
    #:make-identifier-form make-identifier-expression
    #:relative-precedence expression-relative-precedence)

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
    #:in-space in-binding-space
    #:prefix-operator-ref binding-prefix-operator-ref
    #:infix-operator-ref binding-infix-operator-ref
    #:check-result check-binding-result
    #:make-identifier-form make-identifier-binding
    #:relative-precedence binding-relative-precedence
    #:name-root-ref name-root-binding-ref))

;; For a module top level, interleaves expansion and enforestation:
(define-syntax (rhombus-top stx)
  (syntax-parse stx
    [(_ . rest) #'(rhombus-top-step rhombus-top #t () . rest)]))

;; For a nested context
(define-syntax (rhombus-nested stx)
  (syntax-parse stx
    [(_ . rest) #'(rhombus-top-step rhombus-nested #f () . rest)]))

;; Trampoline variant where `top` for return is provided first
(define-syntax (rhombus-top-step stx)
  (with-syntax-error-respan
    (syntax-local-introduce
     (syntax-parse (syntax-local-introduce stx)
       [(_ top decl-ok? data) #`(begin)]
       [(_ top decl-ok? (data ...) ((~datum group) ((~datum parsed) decl)) . forms)
        #`(begin decl (top data ... . forms))]
       ;; note that we may perform hierarchical name resolution
       ;; up to four times, since resolution in `:declaration`,
       ;; `:definition`, etc., doesn't carry over
       [(_ top decl-ok? (data ...) e::definition-sequence . tail)
        (define-values (parsed new-tail)
          (apply-definition-sequence-transformer #'e.id #'e.tail #'tail))
        #`(begin (begin . #,parsed) (top data ... . #,new-tail))]
       [(_ top decl-ok? (data ...) form . forms)
        (define (nestable-parsed)
          (syntax-parse #'form
            [e::nestable-declaration #'(begin . e.parsed)]
            [e::definition #'(begin . e.parsed)]
            [e::expression (syntax/loc #'e.parsed (#%expression e.parsed))]))
        (define parsed
          (if (syntax-e #'decl-ok?)
              (syntax-parse #'form
                [e::declaration #'(begin . e.parsed)]
                [_ (nestable-parsed)])
              (nestable-parsed)))
        (syntax-parse #'forms
          [() parsed]
          [_ #`(begin #,parsed (top data ... . forms))])]))))

;; For a definition context:
(define-syntax (rhombus-definition stx)
  (with-syntax-error-respan
    (syntax-local-introduce
     (syntax-parse (syntax-local-introduce stx)
       [(_) #'(begin)]
       [(_ ((~datum group) ((~datum parsed) defn))) #'defn]
       [(_ e::definition) #'(begin . e.parsed)]
       [(_ e::expression) #'(#%expression e.parsed)]))))

;; For an expression context, interleaves expansion and enforestation:
(define-syntax (rhombus-body stx)
  (syntax-parse stx
    [(_)
     (raise-syntax-error #f "block has no expressions" stx)]
    [(_ . tail)
     #`(let ()
         (rhombus-forwarding-sequence
          #:block #:need-end-expr #,stx
          (rhombus-body-sequence . tail)))]))

;; Similar to `rhombus-body`, but goes through `#%body`:
(define-syntax (rhombus-body-at stx)
  (syntax-parse stx
    [(_ tag . tail)
     (quasisyntax/loc #'tag
       (rhombus-expression (group #,(datum->syntax #'tag '#%body #'tag #'tag)
                                  (#,(syntax-property (datum->syntax #f 'block #'tag) 'raw "")
                                   . tail))))]))

;; Like `(rhombus-expression (group _))`, but recognizes `block` forms
(define-syntax (rhombus-body-expression stx)
  (syntax-parse stx
    [(_ ((~and body-tag (~datum block)) body ...))
     #'(rhombus-body-at body-tag body ...)]
    [(_ ((~datum parsed) rhs))
     #'rhs]
    [(_ rhs)
     #'(rhombus-expression (group rhs))]))

;; For a definition context, interleaves expansion and enforestation:
(define-syntax (rhombus-body-sequence stx)
  (with-syntax-error-respan
    (syntax-parse (syntax-local-introduce stx)
      [(_) #'(begin)]
      [(_ e::definition-sequence . tail)
       (define-values (parsed new-tail)
         (apply-definition-sequence-transformer #'e.id #'e.tail #'tail))
       (syntax-local-introduce
        #`(begin
            (begin . #,parsed)
            (rhombus-body-sequence . #,new-tail)))]
      [(_ e::definition . tail)
       (syntax-local-introduce
        #`(begin
            (begin . e.parsed)
            (rhombus-body-sequence . tail)))]
      [(_ e::expression . tail)
       (syntax-local-introduce
        #`(begin
            (#%expression e.parsed)
            (rhombus-body-sequence . tail)))])))

;; For an expression context:
(define-syntax (rhombus-expression stx)
  (enforest-rhombus-expression stx))

(define-for-syntax (enforest-rhombus-expression stx)
  (with-syntax-error-respan
    (syntax-parse (syntax-local-introduce stx)
      [(_ e::expression) (syntax-local-introduce #'e.parsed)])))

;; If `e` is a block with a single group, and if the group is not a
;; definition, then parse the expression
(define-for-syntax (enforest-expression-block e)
  (syntax-parse e
    [((~datum block) g)
     (cond
       [(definition? #'g) #`(rhombus-body-expression #,e)]
       [else
        (syntax-parse #'g
          [g-e::expression #'g-e.parsed])])]
    [((~datum group) . _)
     (syntax-parse e
       [g-e::expression #'g-e.parsed])]
    [((~datum block) g ...) #`(rhombus-body g ...)]))

;; Forces enforestation through `rhombus-expression`; note that
;; `#%parens` eagerly enforests its content, so this effectively
;; goes eagerly through parentheses
(define-for-syntax (rhombus-local-expand stx)
  (syntax-parse stx
    #:literals (rhombus-expression)
    [(rhombus-expression . _)
     (enforest-rhombus-expression stx)]
    [_ stx]))
