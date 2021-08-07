#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     enforest/proc-name
                     "srcloc.rkt"
                     "tail.rkt")
         "name-root.rkt"
         "declaration.rkt"
         "syntax.rkt"
         "parse.rkt")

(provide decl)

(define-syntax decl
  (simple-name-root macro))

(define-syntax macro
  (make-identifier-syntax-definition-transformer (lambda (x) x)
                                                 #'make-declaration-transformer))

(define-for-syntax (make-declaration-transformer proc)
  (declaration-transformer
   (lambda (tail)
     (syntax-parse tail
       [(head . tail)
        (unpack-declarations (proc (pack-tail #'tail) #'head) proc)]))))

(define-for-syntax (unpack-declarations form proc)
  (syntax-parse form
    #:datum-literals (block group)
    [(block (group d ...) ...)
     #`((rhombus-top (group d ...))
        ...)]
    [_ (raise-result-error (proc-name proc) "declaration-list?" form)]))
