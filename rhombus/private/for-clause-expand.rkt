#lang racket/base
(require (for-syntax racket/base
                     version/utils))

;; temporary workaround for older Racket versions

(define-syntax (pick stx)
  (if (version<? (version) "8.11.1.4")
      #'(begin
          (define (syntax-local-splicing-for-clause-introduce stx)
            stx)
          (provide syntax-local-splicing-for-clause-introduce))
      #'(begin
          (require racket/for-clause)
          (provide syntax-local-splicing-for-clause-introduce))))

(pick)
