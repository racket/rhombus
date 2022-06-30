#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         (submod "import.rkt" for-meta)
         "syntax.rkt"
         "parse.rkt")

(provide imp)

(define-simple-name-root imp
  modifier)

(define-identifier-syntax-definition-transformer modifier
  (lambda (x) x)
  #'make-import-modifier)

(define-for-syntax (make-import-modifier proc)
  (import-modifier
   (lambda (req stx)
     (error "TBD"))))
