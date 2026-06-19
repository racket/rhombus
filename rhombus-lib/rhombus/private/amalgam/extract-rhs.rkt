#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "parens.rkt")

(provide (for-syntax extract-rhs))

(define-for-syntax (extract-rhs b)
  (syntax-parse b
    [#:none b]
    [#:error b]
    [[#:stx stx (_::block g)] #'[#:stx stx g]]
    [[#:stx stx b]
     (raise-syntax-error #f
                         "expected a single entry point in block body"
                         #'stx
                         #'b)]
    [(_::block g) #'g]
    [b (raise-syntax-error #f
                           "expected a single entry point in block body"
                           #'b)]))
