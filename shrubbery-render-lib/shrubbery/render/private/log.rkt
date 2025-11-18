#lang racket/base

(provide (all-defined-out))

(define-logger shrubbery-render)

(define (format-log . args)
  (apply
   string-append
   (let loop ([args args])
     (cond
       [(null? args) null]
       [(and (symbol? (car args))
             (pair? (cdr args)))
        (cons (format "\n  ~a: ~v"
                      (car args)
                      (cadr args))
              (loop (cddr args)))]
       [else
        (cons (format "\n  ~v"
                      (car args))
              (loop (cdr args)))]))))
