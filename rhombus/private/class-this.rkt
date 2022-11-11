#lang racket/base
(require (for-syntax racket/base)
         racket/stxparam)

(provide this-id)

(define-syntax-parameter this-id #f)
