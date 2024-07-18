#lang racket/base
(require scribble/manual
         (for-label racket/base))

(provide char_is_symbolic)

(define char_is_symbolic (racket char-symbolic?))
