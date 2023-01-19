#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         (submod "export.rkt" for-meta)
         "macro-macro.rkt"
         "parse.rkt")

(provide expo)

(define-simple-name-root expo
  modifier
  only)

(define-name-root only
  #:fields
  ([modifier modifier-only]))

(define-identifier-syntax-definition-transformer+only modifier modifier-only
  rhombus/expo
  #'make-export-modifier)

(define-for-syntax (make-import-modifier proc)
  (export-modifier
   (lambda (req stx)
     (error "TBD"))))
