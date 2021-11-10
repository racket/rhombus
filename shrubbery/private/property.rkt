#lang racket/base

(provide syntax-raw-property
         syntax-raw-prefix-property
         syntax-raw-suffix-property
         syntax-raw-tail-property)

(define syntax-raw-property
  (case-lambda
    [(stx) (syntax-property stx 'raw)]
    [(stx val) (syntax-property stx 'raw val #t)]))

(define syntax-raw-prefix-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-prefix)]
    [(stx val) (syntax-property stx 'raw-prefix val #t)]))

(define syntax-raw-suffix-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-suffix)]
    [(stx val) (syntax-property stx 'raw-suffix val #t)]))

;; "tail" is attached to the head term of a list, and it
;; applies after the last item in the list
(define syntax-raw-tail-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-tail)]
    [(stx val) (syntax-property stx 'raw-tail val #t)]))
