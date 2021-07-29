#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     enforest/proc-name
                     "srcloc.rkt"
                     "tail.rkt")
         "definition.rkt"
         "syntax.rkt"
         "parse.rkt")

(provide definition_macro)

(define-syntax definition_macro
  (make-identifier-syntax-definition-transformer (lambda (x) x)
                                                 #'make-definition-transformer))

(define-for-syntax (make-definition-transformer proc)
  (definition-transformer
   (lambda (tail)
     (define defns (syntax-parse tail
                     [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (unpack-definitions defns proc))))

(define-for-syntax (unpack-definitions form proc)
  (syntax-parse form
    #:datum-literals (block group)
    [(block (group d ...) ...)
     #`((rhombus-definition (group d ...))
        ...)]
    [_ (raise-result-error (proc-name proc) "definition-list?" form)]))
