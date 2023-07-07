#lang racket/base
(require racket/symbol)

(provide full-space-name
         add-space)

(define (full-space-name space-name)
  (case space-name
    [(#f var datum value hide) space-name]
    [(expr) #f]
    [else
     (define str (symbol->immutable-string space-name))
     (if (regexp-match? #rx"/" str)
         space-name
         (string->symbol (string-append "rhombus/" (symbol->string space-name))))]))

(define (add-space stx space-name/full)
  (cond
    [(eq? space-name/full 'hide)
     (quote-syntax never-bound)]
    [else
     (define space
       (case space-name/full
         [(#f var value datum expr) #f]
         [else space-name/full]))
     (if space
         ((make-interned-syntax-introducer space) stx 'add)
         stx)]))
