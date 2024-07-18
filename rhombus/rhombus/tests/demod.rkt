#lang racket/base

(define src (collection-file-path "amalgam.rkt" "rhombus/private"))
(define exp
  (parameterize ([read-accept-reader #t])
    (call-with-input-file* src read)))

(let loop ([exp exp])
  (cond
    [(eq? exp '#:no-demod)
     (error "amalgam is in `#:no-demod` mode")]
    [(pair? exp)
     (loop (car exp))
     (loop (cdr exp))]))
