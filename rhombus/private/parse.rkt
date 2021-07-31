#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest
                     enforest/transformer
                     "srcloc.rkt")
         "forwarding-sequence.rkt"
         "declaration.rkt"
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "srcloc.rkt")

(provide rhombus-top
         rhombus-definition
         rhombus-block
         rhombus-expression

         rhombus-block-at

         (for-syntax :declaration
                     :definition
                     :expression
                     :binding

                     ;; for continuing enforestation of expressions or bindings:
                     :prefix-op+expression+tail
                     :infix-op+expression+tail
                     :prefix-op+binding+tail
                     :infix-op+binding+tail))

(begin-for-syntax
  ;; Form at the top of a module:
  (define-transform-class :declaration
    "declaration"
    declaration-transformer-ref
    check-declaration-result)

  ;; Form in a definition context:
  (define-transform-class :definition
    "definition"
    definition-transformer-ref
    check-definition-result)

  ;; Form in an expression context:
  (define-enforest enforest-expression enforest-expression-step
    :expression :prefix-op+expression+tail :infix-op+expression+tail
    "expression" "expression operator"
    in-expression-space
    expression-prefix-operator-ref expression-infix-operator-ref
    check-expression-result
    make-identifier-expression)

  ;; Form in a binding context:
  (define-enforest enforest-binding enforest-binding-step
    :binding :prefix-op+binding+tail :infix-op+binding+tail
    "binding" "binding operator"
    in-binding-space
    binding-prefix-operator-ref binding-infix-operator-ref
    check-binding-result
    make-identifier-binding))

;; For a module top level, interleaves expansion and enforestation:
(define-syntax (rhombus-top stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ form . forms)
     (define parsed
       (with-syntax-error-respan
         (syntax-local-introduce
          (syntax-parse (syntax-local-introduce #'form)
            [e::declaration #'(begin . e.parsed)]
            [e::definition #'(begin . e.parsed)]
            [e::expression #'(#%expression e.parsed)]))))
     (syntax-parse #'forms
       [() parsed]
       [_ #`(begin #,parsed (rhombus-top . forms))])]))

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
(define-syntax (rhombus-block stx)
  (syntax-parse stx
    [(_)
     (raise-syntax-error #f "block has no expressions" stx)]
    [(_ . tail)
     #`(let ()
         (rhombus-forwarding-sequence
          #:need-end-expr #,stx
          (rhombus-body . tail)))]))

(define-syntax (rhombus-block-at stx)
  (syntax-parse stx
    [(_ tag . tail)
     (syntax/loc #'tag (rhombus-block . tail))]))

;; For an expression context, interleaves expansion and enforestation:
(define-syntax (rhombus-body stx)
  (with-syntax-error-respan
    (syntax-parse (syntax-local-introduce stx)
      [(_) #'(begin)]
      [(_ e::definition . tail)
       (syntax-local-introduce
        #`(begin
            (begin . e.parsed)
            (rhombus-body . tail)))]
      [(_ e::expression . tail)
       (syntax-local-introduce
        #`(begin
            (#%expression e.parsed)
            (rhombus-body . tail)))])))

;; For an expression context:
(define-syntax (rhombus-expression stx)
  (with-syntax-error-respan
    (syntax-parse (syntax-local-introduce stx)
      [(_ e::expression) (syntax-local-introduce #'e.parsed)])))
