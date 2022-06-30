#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         "declaration.rkt"
         "syntax.rkt"
         "parse.rkt"
         "implicit.rkt")

(provide decl)

(define-simple-name-root decl
  macro)

(define-identifier-syntax-definition-transformer macro
  (lambda (x) x)
  #'make-declaration-transformer)

(define-for-syntax (make-declaration-transformer proc)
  (declaration-transformer
   (lambda (tail)
     (syntax-parse tail
       [(head . tail)
        (unpack-declarations (proc (pack-tail #'tail) #'head) proc)]))))

(define-for-syntax (unpack-declarations form proc)
  (syntax-parse (unpack-multi form proc)
    #:datum-literals (parens block group)
    [((group d ...) ...)
     #`((rhombus-top (group d ...))
        ...)]
    [_ (raise-result-error (proc-name proc) "declaration-list?" form)]))
