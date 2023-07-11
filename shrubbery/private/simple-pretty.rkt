#lang racket/base

(provide render-pretty)

;; A dumb "pretty" printer that always uses either the 1-line mode or
;; maximal-line mode, on the assumption that the first branch of each
;; `or` leads to the former and the latter branch leads to the latter.

;; This printer doesn't currently support a 'flatten operation as
;; sometimes appears in the literature, because `pretty-shrubbery`
;; doesn't generate one.

(define (render-pretty doc o #:multi-line? [multi-line? #f])
  (let loop ([doc doc] [col 0] [indent 0])
    (cond
      [(string? doc) (display doc o) (+ col (string-length doc))]
      [(bytes? doc) (display doc o) (+ col (bytes-utf-8-length doc))]
      [(eq? doc 'nl)
       (newline o)
       (display (make-string indent #\space) o)
       indent]
      [(not (and (pair? doc) (list? doc)))
       (error 'render-pretty "bad format ~v" doc)]
      [(eq? (car doc) 'seq)
       (for/fold ([col col]) ([doc (in-list (cdr doc))])
         (loop doc col indent))]
      [(and (eq? (car doc) 'nest)
            (= (length doc) 3))
       (loop (caddr doc) col (+ indent (cadr doc)))]
      [(and (eq? (car doc) 'align)
            (= (length doc) 2))
       (loop (cadr doc) col col)]
      [(and (eq? (car doc) 'or)
            (= (length doc) 3))
       (if multi-line?
           (loop (caddr doc) col indent)
           (loop (cadr doc) col indent))]
      [else
       (error 'render-pretty "bad format ~v" doc)])))
