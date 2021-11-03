#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest
                     enforest/transformer
                     enforest/sequence
                     "srcloc.rkt"
                     "name-path-op.rkt")
         "forwarding-sequence.rkt"
         "declaration.rkt"
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "srcloc.rkt")

(provide rhombus-top
         rhombus-definition
         rhombus-body
         rhombus-expression

         rhombus-body-at
         rhombus-body-expression
         rhombus-body-sequence

         (for-syntax :declaration
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
                     binding-relative-precedence))

(begin-for-syntax
  ;; Form at the top of a module:
  (define-transform
    #:syntax-class :declaration
    #:desc "declaration"
    #:name-path-op name-path-op
    #:transformer-ref declaration-transformer-ref
    #:check-result check-declaration-result)

  ;; Form in a definition context:
  (define-transform
    #:syntax-class :definition
    #:predicate definition?
    #:desc "definition"
    #:name-path-op name-path-op
    #:transformer-ref definition-transformer-ref
    #:check-result check-definition-result)

  ;; Form in a definition context that can consume extra groups:
  (define-sequence-transform
    #:syntax-class :definition-sequence
    #:predicate definition-sequence?
    #:desc "definition sequence"
    #:name-path-op name-path-op
    #:transformer-ref definition-sequence-transformer-ref
    #:check-result check-definition-result)

  ;; Form in an expression context:
  (define-enforest
    #:syntax-class :expression
    #:prefix-more-syntax-class :prefix-op+expression+tail
    #:infix-more-syntax-class :infix-op+expression+tail
    #:desc "expression"
    #:operator-desc "expression operator"
    #:in-space in-expression-space
    #:name-path-op name-path-op
    #:prefix-operator-ref expression-prefix-operator-ref
    #:infix-operator-ref expression-infix-operator-ref
    #:check-result check-expression-result
    #:make-identifier-form make-identifier-expression
    #:relative-precedence expression-relative-precedence)

  ;; Form in a binding context:
  (define-enforest
    #:syntax-class :binding
    #:prefix-more-syntax-class :prefix-op+binding+tail
    #:infix-more-syntax-class :infix-op+binding+tail
    #:desc "binding"
    #:operator-desc "binding operator"
    #:in-space in-binding-space
    #:name-path-op name-path-op
    #:prefix-operator-ref binding-prefix-operator-ref
    #:infix-operator-ref binding-infix-operator-ref
    #:check-result check-binding-result
    #:make-identifier-form make-identifier-binding
    #:relative-precedence binding-relative-precedence))

;; For a module top level, interleaves expansion and enforestation:
(define-syntax (rhombus-top stx)
  (with-syntax-error-respan
    (syntax-local-introduce
     (syntax-parse (syntax-local-introduce stx)
       [(_) #'(begin)]
       ;; note that we may perform hierarchical name resolution
       ;; up to four times, since resolution in `:declaration`,
       ;; `:definition`, etc., doesn't carry over
       [(_ e::definition-sequence)
        #`(begin (begin . e.parsed) (rhombus-top . e.tail))]
       [(_ form . forms)
        (define parsed
          (syntax-parse #'form
            [e::declaration #'(begin . e.parsed)]
            [e::definition #'(begin . e.parsed)]
            [e::expression #'(#%expression e.parsed)]))
        (syntax-parse #'forms
          [() parsed]
          [_ #`(begin #,parsed (rhombus-top . forms))])]))))

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
          #:need-end-expr #,stx
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
      [(_ e::definition-sequence)
       (syntax-local-introduce
        #`(begin
            (begin . e.parsed)
            (rhombus-body-sequence . e.tail)))]
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
    [_ #`(rhombus-expression (group #,e))]))
