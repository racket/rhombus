#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     enforest/proc-name
                     "srcloc.rkt"
                     "tail.rkt")
         "name-root.rkt"
         (submod "import.rkt" for-meta)
         "syntax.rkt"
         "parse.rkt")

(provide imp)

(define-syntax imp
  (simple-name-root modifier))

(define-syntax modifier
  (make-identifier-syntax-definition-transformer (lambda (x) x)
                                                 #'make-import-modifier))

(define-for-syntax (make-import-modifier proc)
  (import-modifier
   (lambda (req stx)
     (error "TBD"))))
