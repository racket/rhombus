#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "parse.rkt")
         (for-meta 2 racket/base)
         "definition.rkt")

(provide begin_for_meta)

(define-syntax begin_for_meta
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (block)
        [(_ (block groups ...))
         (list #`(begin-for-syntax
                   (rhombus-top groups ...)))]
        [_
         (raise-syntax-error #f "expected block" stx)]))))