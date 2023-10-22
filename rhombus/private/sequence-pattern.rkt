#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide (for-syntax is-sequence-pattern?))

;; Make sure that any pattern matching a sequence of terms is
;; wrapped with `~seq`, so we can recognize it
(define-for-syntax (is-sequence-pattern? pat)
  (syntax-parse pat
    [((~datum ~seq) . _) #t]
    [_ #f]))
