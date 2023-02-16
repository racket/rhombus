#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
                     (for-syntax racket/base))
         "space-provide.rkt"
         "name-root.rkt"
         "declaration.rkt"
         "nestable-declaration.rkt"
         "macro-macro.rkt"
         "parse.rkt"
         "implicit.rkt")

(define+provide-space decl #f
  #:fields
  (macro
   nestable_macro))

(provide (for-syntax (for-space rhombus/namespace
                                decl_meta)))

(define-identifier-syntax-definition-transformer macro
  #f
  #'make-declaration-transformer)

(define-identifier-syntax-definition-transformer nestable_macro
  #f
  #'make-nestable-declaration-transformer)

(define-for-syntax (make-declaration-transformer proc)
  (declaration-transformer
   (lambda (tail)
     (syntax-parse tail
       [(head . tail)
        (unpack-declarations (proc (pack-tail #'tail) #'head) proc)]))))

(define-for-syntax (make-nestable-declaration-transformer proc)
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

;; ----------------------------------------

(begin-for-syntax
  (define-name-root decl_meta
    #:fields
    (Group
     NestableGroup)))

(begin-for-syntax
  (define-syntax-class :is_declaration
    #:attributes ()
    (pattern g
             #:when (or (declaration? #'g)
                        (nestable-declaration? #'g))))
  (define-syntax-class :is_nestedable_declaration
    #:attributes ()
    (pattern g
             #:when (nestable-declaration? #'g)))
  
  (define-syntax-class-syntax Group
    (make-syntax-class #':is_declaration
                       #:kind 'group
                       #:fields #'()))
  
  (define-syntax-class-syntax NestableGroup
    (make-syntax-class #':is_nestable_declaration
                       #:kind 'group
                       #:fields #'())))
