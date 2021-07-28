#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     "transformer.rkt"
                     "srcloc.rkt"
                     "check.rkt"
                     "tail.rkt")
         "syntax.rkt"
         "parse.rkt")

(provide definition)

(define-syntax definition
  (make-identifier-syntax-definition-transformer (lambda (x) x)
                                                 #'make-definition-transformer))

(define-for-syntax (make-definition-transformer proc)
  (definition-transformer
   (lambda (tail)
     (define-values (defns exprs)
       (call-with-values
        (lambda ()
          (syntax-parse tail
            [(head . tail) (proc (pack-tail #'tail) #'head)]))
        (case-lambda
          [(defns) (values defns #'(parens))]
          [(defns exprs) (values defns exprs)])))
     (values (unpack-definitions defns proc)
             (unpack-expressions exprs proc)))))

(define-for-syntax (unpack-definitions form proc)
  (syntax-parse form
    #:datum-literals (block group)
    [(block (group d ...) ...)
     #`((rhombus-definition (group d ...))
        ...)]
    [_ (raise-result-error (proc-name proc) "definition-list?" form)]))

(define-for-syntax (unpack-expressions form proc)
  (syntax-parse form
    #:datum-literals (parens group)
    [(parens (group e ...) ...)
     #`((rhombus-expression (group e ...))
        ...)]
    [_ (raise-result-error (proc-name proc) "expression-list?" form)]))
