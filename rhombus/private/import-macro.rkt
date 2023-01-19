#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         (submod "import.rkt" for-meta)
         "macro-macro.rkt"
         "parse.rkt")

(provide impo)

(define-simple-name-root impo
  modifier
  only)

(define-name-root only
  #:fields
  ([modifier modifier-only]))

(define-identifier-syntax-definition-transformer+only modifier modifier-only
  rhombus/impo
  #'make-import-modifier)

(define-for-syntax (make-import-modifier proc)
  (import-modifier
   (lambda (req stx)
     (error "TBD"))))
