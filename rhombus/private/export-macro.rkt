#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "space-provide.rkt"
         "name-root.rkt"
         (submod "export.rkt" for-meta)
         "space.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space expo rhombus/expo
  #:fields
  (modifier))

(define-identifier-syntax-definition-transformer modifier
  rhombus/expo
  #'make-export-modifier)

(define-for-syntax (make-import-modifier proc)
  (export-modifier
   (lambda (req stx)
     (error "TBD"))))
