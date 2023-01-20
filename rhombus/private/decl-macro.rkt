#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "space-provide.rkt"
         "name-root.rkt"
         "declaration.rkt"
         "macro-macro.rkt"
         "parse.rkt"
         "implicit.rkt")

(define+provide-space decl #f
  #:fields
  (macro))

(define-identifier-syntax-definition-transformer macro
  #f
  #'make-declaration-transformer)

(define-for-syntax (make-declaration-transformer proc)
  (declaration-transformer
   (lambda (tail)
     (syntax-parse tail
       [(head . tail)
        (unpack-declarations (proc (pack-tail #'tail) #'head) proc)]))))

(define-for-syntax (unpack-declarations form proc)
  (syntax-parse (unpack-multi form proc #f)
    #:datum-literals (parens block group)
    [((group d ...) ...)
     #`((rhombus-top (group d ...))
        ...)]
    [_ (raise-result-error (proc-name proc) "declaration-list?" form)]))
