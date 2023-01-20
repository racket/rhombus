#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/provide-transform))

(provide for-spaces)

(define-syntax for-spaces
  (make-provide-pre-transformer
   (lambda (stx space+phases)
     (syntax-parse stx
       [(_ (space ...) out ...)
        #`(combine-out (for-space space out ...)
                       ...)]))))
