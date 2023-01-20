#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "space-provide.rkt"
         "name-root.rkt"
         (submod "import.rkt" for-meta)
         "space.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space impo rhombus/impo
  #:fields
  (modifier))

(define-identifier-syntax-definition-transformer modifier
  rhombus/impo
  #'make-import-modifier)

(define-for-syntax (make-import-modifier proc)
  (import-modifier
   (lambda (req stx)
     (error "TBD"))))
