#lang racket/base
(require shrubbery/property)

(provide group-tag)

(define group-tag (syntax-raw-property (datum->syntax #f 'group) ""))
