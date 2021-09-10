#lang racket/base

(provide syntax-raw-property
         syntax-raw-prefix-property
         syntax-raw-tail-property)

(define syntax-raw-property
  (case-lambda
    [(stx) (syntax-property stx 'raw)]
    [(stx val) (syntax-property stx 'raw val #t)]))

(define syntax-raw-prefix-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-prefix)]
    [(stx val) (syntax-property stx 'raw-prefix val #t)]))

(define syntax-raw-tail-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-tail)]
    [(stx val) (syntax-property stx 'raw-tail val #t)]))
