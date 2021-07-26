#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "parse.rkt"
         "expression.rkt")

(provide ?
         ¿
         (rename-out [¿ ??]))

(define-for-syntax (escape e [depth 0])
  (syntax-parse e
    [((~and tag (~or (~datum parens) (~datum braces) (~datum block) (~datum alts)))
      g ...)
     (with-syntax ([(new-g ...) (map (escape-group depth) (syntax->list #'(g ...)))])
       (syntax/loc e
         (tag new-g ...)))]
    [_ e]))

(define-for-syntax ((escape-group depth) g)
  (syntax-parse g
    [((~and tag (~datum group)) e ...)
     (with-syntax ([(new-e ...)
                    (let loop ([es #'(e ...)])
                      (syntax-parse es
                        [() null]
                        [((~and op ((~datum op) (~literal ¿))) e . tail)
                         (if (zero? depth)
                             (cons #'(unsyntax (rhombus-expression (group e))) (loop #'tail))
                             (list* #'op (escape #'e (sub1 depth)) (loop #'tail)))]
                        [((~and op ((~datum op) (~literal ?))) e . tail)
                         (list* #'op (escape #'e (add1 depth)) (loop #'tail))]
                        [(e . tail)
                         (cons (escape #'e depth) (loop #'tail))]))])
     (syntax/loc g (tag new-e ...)))]))

(define-syntax ?
  (expression-prefix-operator
   (quote-syntax ?)
   '((default . stronger))
   #t ; transformer
   (lambda (stx)
     (syntax-parse stx
       [(op e . tail)
        (values (relocate (span-srcloc #'op #'parens-tag)
                          #`(#,(quote-syntax quasisyntax) #,(escape #'e)))
                #'tail)]))))

(define-syntax ¿
  (expression-prefix-operator
   (quote-syntax ¿)
   '((default . stronger))
   #t ; transformer
   (lambda (stx)
     (syntax-parse stx
       [(op . _)
        (raise-syntax-error #f
                            "misuse outside of ?"
                            #'op)]))))
