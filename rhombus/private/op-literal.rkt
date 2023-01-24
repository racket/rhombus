#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse)
         (rename-in "ellipsis.rkt"
                    [... rhombus...])
         (rename-in "underscore.rkt"
                    [_ rhombus_])
         (rename-in "equal.rkt"
                    [= rhombus=])
         "dollar.rkt"
         "rest-marker.rkt"
         "assign.rkt"
         "expression.rkt"
         "binding.rkt")

(provide (for-syntax
          :$ :$-expr :$-bind
          :... :...-expr :...-bind
          :_ :_-expr :_-bind
          :& :&-expr :&-bind
          :~& :~&-expr :~&-bind
          := :=-expr :=-bind
          ::= ::=-expr ::=-bind))

(begin-for-syntax
  (define-syntax-rule (define-literal-class id id-expr id-bind orig-id desc)
    (begin
      (define-syntax-class (id in-space)
        #:attributes (name)
        #:description desc
        [pattern op::name
                 #:when (free-identifier=? (in-space #'orig-id) (in-space #'op.name))
                 #:attr name #'op.name])
      (define-syntax-class id-expr
        #:attributes (name)
        #:description desc
        [pattern op::name
                 #:when (free-identifier=? (expr-quote orig-id) #'op.name)
                 #:attr name #'op.name])
      (define-syntax-class id-bind
        #:attributes (name)
        #:description desc
        [pattern op::name
                 #:when (free-identifier=? (bind-quote orig-id) (in-binding-space #'op.name))
                 #:attr name #'op.name])))

  (define-literal-class :$ :$-expr :$-bind $ "an escape operator")
  (define-literal-class :... :...-expr :...-bind rhombus... "an ellipsis operator")
  (define-literal-class :_ :_-expr :_-bind rhombus_ "a wildcard operator")
  (define-literal-class :& :&-expr :&-bind & "a splice operator")
  (define-literal-class :~& :~&-expr :~&-bind ~& "a keyword splice operator")
  (define-literal-class := :=-expr :=-bind rhombus= "an equal operator")
  (define-literal-class ::= ::=-expr ::=-bind := "an assignment operator"))
