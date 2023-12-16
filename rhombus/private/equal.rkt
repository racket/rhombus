#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse)
         "provide.rkt"
         "expression.rkt"
         "binding.rkt")

(provide (for-spaces (#f
                      rhombus/bind)
                     (rename-out [rhombus= =])))

(module+ for-parse
  (provide (for-syntax :equal
                       :not-equal
                       raise-too-many-equals)))

(define-syntax rhombus=
  (expression-infix-operator
   (expr-quote rhombus=)
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(head::name . _)
        (raise-syntax-error #f
                            "not an expression operator"
                            #'head.name)]))
   'none))

(define-binding-syntax rhombus=
  (binding-infix-operator
   (bind-quote rhombus=)
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(head::name . _)
        (raise-syntax-error #f
                            "not a binding operator"
                            #'head.name)]))
   'none))

(begin-for-syntax
  (define-syntax-class :equal
    #:attributes (name)
    #:description "equal operator"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? #'name
                                       (expr-quote rhombus=))))
  (define-syntax-class :not-equal
    #:attributes ()
    (pattern (~not _::equal))))

(define-for-syntax (raise-too-many-equals stx a b)
  (raise-syntax-error #f
                      (string-append "multiple immediate equals not allowed in this group"
                                     "\n use parentheses to disambiguate")
                      stx
                      a
                      (list b)))
