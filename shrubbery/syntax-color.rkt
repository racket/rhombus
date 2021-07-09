#lang racket/base
(require "lex.rkt"
         syntax-color/racket-lexer)

(provide shrubbery-lexer)

(define (shrubbery-lexer in pos status)
  (lex/status in pos status racket-lexer/status))
