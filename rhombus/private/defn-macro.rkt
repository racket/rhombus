#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt"
                     "pack-s-exp.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
                     "macro-result.rkt"
                     (for-syntax racket/base))
         "space-provide.rkt"
         "name-root.rkt"
         "definition.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space defn #f
  #:fields
  (macro
   sequence_macro))

(provide (for-syntax (for-space rhombus/namespace
                                defn_meta)))

;; ----------------------------------------

(define-identifier-syntax-definition-transformer macro
  #f
  #'make-definition-transformer)

(define-for-syntax (make-definition-transformer proc)
  (definition-transformer
   (lambda (stx)
     (define defns (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (unpack-definitions defns proc))))

(define-for-syntax (unpack-definitions form proc)
  (syntax-parse (and (syntax? form) (unpack-multi form proc #f))
    [(g ...)
     #`((rhombus-definition g)
        ...)]
    [_ (raise-bad-macro-result (proc-name proc) "definitions and expressions" form)]))

;; ----------------------------------------

(define-identifier-syntax-definition-sequence-transformer sequence_macro
  #f
  #'make-definition-sequence-transformer)

(define-for-syntax (make-definition-sequence-transformer proc)
  (definition-sequence-transformer
   (lambda (stx tail)
     (define-values (defns new-tail)
       (syntax-parse stx
         [(head . h-tail) (proc (pack-tail #'h-tail) (pack-multi tail) #'head)]))
     (values (unpack-definitions defns proc)
             (unpack-multi new-tail proc #f)))))

;; ----------------------------------------

(begin-for-syntax
  (define-name-root defn_meta
    #:fields
    (Group
     SequenceStartGroup
     pack_s_exp)))

(begin-for-syntax
  (define-syntax-class :is_definition
    #:attributes ()
    (pattern g
             #:when (definition? #'g)))
  (define-syntax-class :is_definition_sequence
    #:attributes ()
    (pattern g
             #:when (definition-sequence? #'(g))))
  
  (define-syntax-class-syntax Group
    (make-syntax-class #':is_definition
                       #:kind 'group
                       #:fields #'()))
  
  (define-syntax-class-syntax SequenceStartGroup
    (make-syntax-class #':is_definition_sequence
                       #:kind 'group
                       #:fields #'())))

(define-for-syntax (pack_s_exp orig-s)
  #`(parsed
     #:rhombus/defn
     #,(pack-s-exp 'defn.pack_s_exp orig-s)))

