#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide (for-syntax :not-block))

(begin-for-syntax
  (define-syntax-class :not-block
    #:datum-literals (op parens braces brackets quotes)
    (pattern _:identifier)
    (pattern _:keyword)
    (pattern (op . _))
    (pattern (parens . _))
    (pattern (braces . _))
    (pattern (brackets . _))
    (pattern (quotes . _))))
