#lang racket/base
(require racket/symbol
         racket/keyword
         (prefix-in rhombus: "print.rkt"))

(provide str)

(define (str lst)
  (apply string-append-immutable (map to-string lst)))

(define (to-string a)
  (cond
    [(string? a) a]
    [(symbol? a) (symbol->immutable-string a)]
    [(keyword? a) (keyword->immutable-string a)]
    [else
     (define o (open-output-string))
     (rhombus:display a o)
     (get-output-string o)]))
