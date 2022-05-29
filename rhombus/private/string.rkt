#lang racket/base
(require racket/symbol
         racket/keyword
         "define-operator.rkt"
         (prefix-in rhombus: "print.rkt"))

(provide &)

(define-infix & append-as-strings
  #:stronger-than (===))

(define (append-as-strings a b)
  (string-append-immutable (to-string a)
                           (to-string b)))

(define (to-string a)
  (cond
    [(string? a) a]
    [(symbol? a) (symbol->immutable-string a)]
    [(keyword? a) (keyword->immutable-string a)]
    [else
     (define o (open-output-string))
     (rhombus:display a o)
     (get-output-string o)]))
