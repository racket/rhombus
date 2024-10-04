#lang racket/base

(provide sorted-list-subset?)

(define (sorted-list-subset? req-kws kws)
  (let loop ([req-kws req-kws] [kws kws])
    (cond
      [(null? req-kws) #t]
      [(null? kws) #f]
      [(eq? (car req-kws) (car kws)) (loop (cdr req-kws) (cdr kws))]
      [else (loop req-kws (cdr kws))])))
