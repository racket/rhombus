#lang racket/base

(provide shrubbery-paren-matches
         shrubbery-quote-matches)

(define shrubbery-paren-matches
  '((|(| |)|)
    (|[| |]|)
    (|{| |}|)
    (« »)))

(define shrubbery-quote-matches
  '(#\"))
