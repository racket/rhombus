#lang racket/base
(require "version-case.rkt")

;; temporary workaround for older Racket versions

(meta-if-version-at-least
 "8.11.1.4"
 (begin
   (require racket/for-clause)
   (provide syntax-local-splicing-for-clause-introduce))
 (begin
   (define (syntax-local-splicing-for-clause-introduce stx)
     stx)
   (provide syntax-local-splicing-for-clause-introduce)))
