#lang racket/base
(require racket/symbol
         racket/keyword
         "provide.rkt"
         "define-operator.rkt"
         (only-in "arithmetic.rkt"
                  ==
                  ===)
         (only-in (submod "print.rkt" for-string)
                  [display rhombus:display]))

(provide (for-spaces (#f
                      rhombus/repet)

                     +&))

(define-infix +& append-as-strings
  #:stronger-than (== ===))

(define (append-as-strings a b)
  (string-append-immutable (to-string a)
                           (to-string b)))

(define (to-string a)
  (cond
    [(string? a) a]
    [(symbol? a) (symbol->immutable-string a)]
    [(keyword? a) (keyword->immutable-string a)]
    [(identifier? a) (symbol->immutable-string (syntax-e a))]
    [else
     (define o (open-output-string))
     (rhombus:display a o)
     (get-output-string o)]))
