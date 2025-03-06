#lang racket/base
(require rhombus/dot
         "posn.rhm")

(define (check got expected)
  (unless (equal? got expected)
    (error "failed: ~s vs. ~s" got expected)))

(check ((dynamic-dot-ref "hello" 'length)) 5)

(check (dynamic-dot-ref (make_posn 10 20) 'x) 10)
