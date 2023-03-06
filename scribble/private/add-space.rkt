#lang racket/base

(provide full-space-name
         add-space)

(define (full-space-name space-name)
  (case space-name
    [(#f var datum hide) space-name]
    [(expr) #f]
    [else (string->symbol (string-append "rhombus/" (symbol->string space-name)))]))

(define (add-space stx space-name/full)
  (cond
    [(eq? space-name/full 'hide)
     (quote-syntax never-bound)]
    [else
     (define space
       (case space-name/full
         [(#f var datum expr) #f]
         [else space-name/full]))
     (if space
         ((make-interned-syntax-introducer space) stx 'add)
         stx)]))
