#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         "definition.rkt"
         "syntax.rkt"
         "parse.rkt")

(provide defn)

(define-simple-name-root defn
  macro
  sequence_macro)

;; ----------------------------------------

(define-identifier-syntax-definition-transformer macro
  (lambda (x) x)
  #'make-definition-transformer)

(define-for-syntax (make-definition-transformer proc)
  (definition-transformer
   (lambda (stx)
     (define defns (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (unpack-definitions defns proc))))

(define-for-syntax (unpack-definitions form proc)
  (syntax-parse (unpack-multi form proc)
    [(g ...)
     #`((rhombus-definition g)
        ...)]
    [_ (raise-result-error (proc-name proc) "definition-list?" form)]))

;; ----------------------------------------

(define-identifier-syntax-definition-sequence-transformer sequence_macro
  (lambda (x) x)
  #'make-definition-sequence-transformer)

(define-for-syntax (make-definition-sequence-transformer proc)
  (definition-sequence-transformer
   (lambda (stx tail)
     (define-values (defns new-tail)
       (syntax-parse stx
         [(head . h-tail) (proc (pack-tail #'h-tail) (pack-multi tail) #'head)]))
     (values (unpack-definitions defns proc)
             (unpack-multi new-tail proc)))))
