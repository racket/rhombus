#lang racket/base
(require "srcloc.rkt")

(provide check-consistent)

(define (check-consistent stx ids what)
  (define the-id (car ids))
  (for ([another-id (in-list (cdr ids))])
    (unless (free-identifier=? the-id another-id)
      (raise-syntax-error #f
                          (format "case ~a does not match initial case ~a"
                                  what
                                  what)
                          stx
                          another-id))))
