#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     "srcloc.rkt")
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         "binding.rkt"
         "literal.rkt"
         (submod "symbol.rkt" for-static-info)
         (submod "keyword.rkt" for-static-info)
         "static-info.rkt")

(provide (for-spaces (#f
                      rhombus/repet
                      rhombus/bind)
                     |#'|))

;; see also "unquote-binding-primitive.rkt"

(define-syntax |#'|
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-name q . tail)
        (define si (check-quotable #'form-name #'q))
        (values (wrap-static-info*
                 (relocate+reraw
                  (respan (datum->syntax #f (list #'form-name #'q)))
                  #'(quote q))
                 si)
                #'tail)]))))

(define-repetition-syntax |#'|
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-name q . tail)
        (define si (check-quotable #'form-name #'q))
        (values (make-repetition-info #'datum
                                      #'()
                                      (syntax/loc stx (quote q))
                                      si
                                      0)
                #'tail)]))))

(define-binding-syntax |#'|
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-name q . tail)
        (check-quotable #'form-name #'q)
        (values (binding-form #'literal-infoer
                              #`([q #,(string-append "#'" (shrubbery-syntax->string #'q))]))
                #'tail)]))))

(define-for-syntax (check-quotable form-name q)
  (define s (syntax-e q))
  (cond
    [(keyword? s) (get-keyword-static-infos)]
    [(symbol? s) (get-symbol-static-infos)]
    [else
     (raise-syntax-error #f
                         "only an identifier or keyword allowed"
                         (datum->syntax #f (list form-name q))
                         q)]))
