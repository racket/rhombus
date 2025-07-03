#lang racket/base
(require "../variant.rkt")

(provide shrubbery-paren-matches
         shrubbery-quote-matches
         make-shrubbery-paren-matches
         make-shrubbery-quote-matches)

(define shrubbery-paren-matches
  '((|(| |)|)
    (|[| |]|)
    (|{| |}|)
    (|'(| |)'|)
    (« »)))

(define shrubbery-quote-matches
  '(#\" #\'))

(define (make-shrubbery-paren-matches #:variant [variant default-variant])
  shrubbery-paren-matches)

(define (make-shrubbery-quote-matches #:variant [variant default-variant])
  shrubbery-quote-matches)
