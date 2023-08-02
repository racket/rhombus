#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         "binding.rkt"
         "literal.rkt")

(provide (for-spaces (#f
                      rhombus/repet
                      rhombus/bind)
                     |#'|))

;; see also "unquote-binding-primitive.rkt"

(define-syntax |#'|
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(form-name q . tail)
        (check-quotable #'form-name #'q)
        (values (syntax/loc stx (quote q))
                #'tail)]))))

(define-repetition-syntax |#'|
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(form-name q . tail)
        (check-quotable #'form-name #'q)
        (values (make-repetition-info #'datum
                                      #'value
                                      (syntax/loc stx (quote q))
                                      0
                                      0
                                      #'()
                                      #t)
                #'tail)]))))

(define-binding-syntax |#'|
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(form-name q . tail)
        (check-quotable #'form-name #'q)
        (values (binding-form #'literal-infoer
                              #'(q))
                #'tail)]))))

(define-for-syntax (check-quotable form-name q)
  (define s (syntax-e q))
  (unless (or (keyword? s)
              (symbol? s))
    (raise-syntax-error #f
                        "only an identifier or keyword allowed"
                        (datum->syntax #f (list form-name q))
                        q)))
