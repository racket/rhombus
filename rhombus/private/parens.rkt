#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide (for-syntax
          :parens
          :block
          :alts
          :braces
          :brackets))

(begin-for-syntax
  (define-syntax-class :parens
    #:description "parentheses"
    #:opaque
    (pattern (~datum parens)))
  (define-syntax-class :block
    #:description "a `:` block"
    #:opaque
    (pattern (~datum block)))
  (define-syntax-class :alts
    #:description "a block of `|` alternatives"
    #:opaque
    (pattern (~datum alts)))
  (define-syntax-class :braces
    #:description "curly braces"
    #:opaque
    (pattern (~datum braces)))
  (define-syntax-class :brackets
    #:description "square brackets"
    #:opaque
    (pattern (~datum brackets))))
