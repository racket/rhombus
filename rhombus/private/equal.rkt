#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse)
         "expression.rkt")

(provide (for-space #f
                    (rename-out [rhombus= =])))

(module+ for-parse
  (provide (for-syntax :equal
                       check-multiple-equals)))

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

(begin-for-syntax
  (define-syntax-class :equal
    #:attributes (name)
    #:description "an equal operator"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? #'name
                                       (expr-quote rhombus=)))))

(define-for-syntax (check-multiple-equals stx)
  (syntax-parse stx
    [(_ _ ... eq::equal (~seq _ ... more::equal) ...+ _ ...)
     (raise-syntax-error #f
                         (string-append "multiple immediate equals not allowed in this group;"
                                        "\n use parentheses to disambiguate")
                         stx
                         #'eq
                         (syntax->list #'(more ...)))]
    [_ (void)]))
