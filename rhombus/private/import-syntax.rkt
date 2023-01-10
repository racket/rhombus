#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         (submod "import.rkt" for-meta)
         "syntax.rkt"
         "parse.rkt")

(provide impo)

(define-simple-name-root impo
  modifier)

(define-identifier-syntax-definition-transformer modifier
  (lambda (x) x)
  #'make-import-modifier)

(define-for-syntax (make-import-modifier proc)
  (import-modifier
   (lambda (req stx)
     (error "TBD"))))
