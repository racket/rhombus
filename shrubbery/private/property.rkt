#lang racket/base

(provide syntax-raw-property
         syntax-raw-prefix-property
         syntax-raw-suffix-property
         syntax-raw-tail-property
         syntax-raw-tail-suffix-property
         syntax-opaque-raw-property)

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
;; applies after the last item in the list, but counts as
;; the representation of the list itself
(define syntax-raw-tail-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-tail)]
    [(stx val) (syntax-property stx 'raw-tail val #t)]))

;; like tail, but for a further suffix that is outside the list
(define syntax-raw-tail-suffix-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-tail-suffix)]
    [(stx val) (syntax-property stx 'raw-tail-suffix val #t)]))

;; Hides any nested syntax and ignores an immediate 'raw property when
;; present and not #f
(define syntax-opaque-raw-property
  (case-lambda
    [(stx) (syntax-property stx 'opaque-raw)]
    [(stx val) (syntax-property stx 'opaque-raw val)]))
