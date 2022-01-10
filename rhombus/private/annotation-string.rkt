#lang racket/base

(provide annotation-any-string
         annotation-string-from-pattern
         annotation-string-to-pattern
         annotation-string-and)

(define annotation-any-string "Any")

(define (annotation-string-from-pattern p)
  (string-append "matching(" p ")"))

(define (annotation-string-to-pattern s)
  (cond
    [(regexp-match #rx"^matching[(](.*)[)]$" s)
     => (lambda (m) (cadr m))]
    [else
     (string-append "(_ :: " s ")")]))

(define (annotation-string-and a b)
  (cond
    [(equal? a annotation-any-string) b]
    [(equal? b annotation-any-string) a]
    [else
     (string-append "and(" a ", " b ")")]))
