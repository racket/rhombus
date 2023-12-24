#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "pack-s-exp.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
                     "macro-result.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     (for-syntax racket/base))
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "declaration.rkt"
         "nestable-declaration.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space decl rhombus/decl
  #:fields
  (macro
   nestable_macro))

(provide (for-syntax (for-space rhombus/namespace
                                decl_meta)))

(define-identifier-syntax-definition-transformer macro
  rhombus/decl
  #'make-declaration-transformer)

(define-identifier-syntax-definition-transformer nestable_macro
  rhombus/decl
  #'make-nestable-declaration-transformer)

(define-for-syntax (make-declaration-transformer proc)
  (declaration-transformer
   (lambda (tail)
     (syntax-parse tail
       [(head . tail)
        (unpack-declarations (proc (pack-tail #'tail) #'head) proc)]))))

(define-for-syntax (make-nestable-declaration-transformer proc)
  (nestable-declaration-transformer
   (lambda (tail)
     (syntax-parse tail
       [(head . tail)
        (unpack-declarations (proc (pack-tail #'tail) #'head) proc)]))))

(define-for-syntax (unpack-declarations form proc)
  (syntax-parse (and (syntax? form) (unpack-multi form proc #f))
    #:datum-literals (group)
    [((group d ...) ...)
     #`((rhombus-top (group d ...))
        ...)]
    [_ (raise-bad-macro-result (proc-name proc) "declarations" form)]))

;; ----------------------------------------

(begin-for-syntax
  (define-name-root decl_meta
    #:fields
    (space
     Group
     NestableGroup
     [pack_s_exp decl_meta.pack_s_exp])))

(define-for-syntax space
  (space-syntax rhombus/decl))

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
                       #:fields #'()))

  (define/arity (decl_meta.pack_s_exp orig-s)
    #:static-infos ((#%call-result #,syntax-static-infos))
    #`(parsed
       #:rhombus/decl
       #,(pack-s-exp who orig-s)))
  )
