#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "enforest.rkt"
                     "op.rkt"
                     (submod "op.rkt" for-parse)
                     "transformer.rkt"
                     (submod "transformer.rkt" for-parse)
                     "check.rkt"
                     "syntax-local.rkt")
         "expression.rkt"
         "binding.rkt")

(provide rhombus-top
         rhombus-definition
         rhombus-block
         rhombus-expression

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
  (define-syntax-class :declaration
    (pattern ((~datum group) head:identifier . tail)
             #:do [(define head-id (transform-in #'head))]
             #:do [(define v (syntax-local-value* head-id declaration-transformer?))]
             #:when (declaration-transformer? v)
             #:attr expandeds (transform-out
                               (apply-declaration-transformer v head-id
                                                              (datum->syntax #f (cons head-id (transform-in #'tail)))))))

  ;; Form in a definition context:
  (define-syntax-class :definition
    (pattern ((~datum group) head:identifier . tail)
             #:do [(define head-id (transform-in #'head))]
             #:do [(define v (syntax-local-value* head-id definition-transformer?))]
             #:when (definition-transformer? v)
             #:do [(define-values (defns-and-exprs exprs)
                     (apply-definition-transformer v head-id (datum->syntax #f (cons head-id (transform-in #'tail)))))]
             #:attr expandeds (transform-out (datum->syntax #f defns-and-exprs))
             #:attr exprs (transform-out (datum->syntax #f exprs))))

  ;; Form in an expression context:
  ;;  :expression is defined via `define-enforest` below

  ;; Form in a binding context:
  ;;  :binding is defined via `define-enforest` below
  
  ;; the expression variant:
  (define-enforest enforest-expression enforest-expression-step
    :expression :prefix-op+expression+tail :infix-op+expression+tail
    "expression" "expression operator"
    in-expression-space
    expression-prefix-operator-ref expression-infix-operator-ref
    check-expression-result
    make-identifier-expression)

  ;; the binding variant:
  (define-enforest enforest-binding enforest-binding-step
    :binding :prefix-op+binding+tail :infix-op+binding+tail
    "binding" "binding operator"
    in-binding-space
    binding-prefix-operator-ref binding-infix-operator-ref
    check-binding-result
    make-identifier-binding))

;; For a module top level, interleaves expansion and enforestation:
(define-syntax (rhombus-top stx)
  (syntax-case stx ()
    [(_) #'(begin)]
    [(_ form . forms)
     #`(begin
         #,(syntax-local-introduce
            (syntax-parse (syntax-local-introduce #'form)
              [e::declaration #'(begin . e.expandeds)]
              [e::definition #'(begin (begin . e.expandeds) . e.exprs)]
              [e::expression #'(#%expression e.expanded)]))
         (rhombus-top . forms))]))

;; For a definition context:
(define-syntax (rhombus-definition stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ ((~datum group) ((~datum parsed) defn))) #'defn]
    [(_ e::definition) #'(begin
                           (begin . e.expandeds)
                           (expression-begin . e.exprs))]
    [(_ e::expression) #'(#%expression e.expanded)]))

;; For an expression context, interleaves expansion and enforestation:
(define-syntax (rhombus-block stx)
  (syntax-parse stx
    [(_)
     (raise-syntax-error #f "found an empty block" stx)]
    [(_ . tail)
     (syntax-local-introduce
      #`(let ()
          . #,(let loop ([tail (syntax-local-introduce #'tail)])
                (syntax-parse tail
                  [() #'()]
                  [(e::definition . tail)
                   (when (and (stx-null? #'tail)
                              (stx-null? #'e.exprs))
                     (raise-syntax-error #f "block does not end with an expression" stx))
                   #`((begin . e.expandeds)
                      (expression-begin . e.exprs)
                      . #,(loop #'tail))]
                  [(e::expression . tail)
                   #`((#%expression e.expanded)
                      . #,(loop #'tail))]))))]))

(define-syntax (expression-begin stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ . exprs) #'(#%expression (begin e.exprs))]))

;; For an expression context:
(define-syntax (rhombus-expression stx)
  (syntax-parse (syntax-local-introduce stx)
    [(_ e::expression) (syntax-local-introduce #'e.expanded)]))
