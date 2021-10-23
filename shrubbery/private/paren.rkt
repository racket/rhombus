#lang racket/base

(provide shrubbery-paren-matches)

(define shrubbery-paren-matches
  '((|(| |)|)
    (|[| |]|)
    (|{| |}|)
    (« »)))
