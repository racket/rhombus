#lang racket/base
(require shrubbery/property)

(provide group-tag
         regroup)

(define group-tag (syntax-raw-property (datum->syntax #f 'group) ""))

(define (regroup stx)
  (datum->syntax #f (cons group-tag (syntax-e stx))))
