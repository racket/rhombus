#lang racket/base
(require (only-in rhombus
                  #%literal
                  +
                  |.|
                  math)
         rhombus/parse
         (only-in racket/math pi))

(define (check got expected)
  (unless (equal? got expected)
    (error "failed: ~s vs. ~s" got expected)))

(check (rhombus-expression (group 1 (op +) 2))
       3)

(check (rhombus-expression (group math (op |.|) pi))
       pi)


