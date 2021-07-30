#lang racket/base
(require (for-syntax racket/base))

(provide (rename-out [rhombus= =]))

(define-syntax rhombus= #f)
