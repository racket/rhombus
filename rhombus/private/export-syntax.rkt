#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         (submod "export.rkt" for-meta)
         "syntax.rkt"
         "parse.rkt")

(provide expo)

(define-simple-name-root expo
  macro
  modifier)

(define-identifier-syntax-definition-transformer modifier
  (lambda (x) x)
  #'make-export-modifier)

(define-for-syntax (make-import-modifier proc)
  (export-modifier
   (lambda (req stx)
     (error "TBD"))))
