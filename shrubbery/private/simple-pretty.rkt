#lang racket/base

(provide render-pretty)

;; A dumb "pretty" printer that always uses either the 1-line mode or
;; maximal-line mode, on the assumption that the first branch of each
;; `or` leads to the former and the latter branch leads to the latter.

;; This printer doesn't currently support a 'flatten operation as
;; sometimes appears in the literature, because that seems imcompatible
;; with shubbery formatting where line breaks matter. But 'flat is
;; supported, which constrains a rendering choice to only flat mode.
;; Shrubbery printing doesn't use 'flat and instead manages the
; constraint directly, but it might be simpler to use it.

(define (render-pretty doc o
                       #:multi-line? [multi-line? #t]
                       #:column [col 0]
                       #:indent [indent 0])
  (define v-ht (make-hasheq))
  (define v-s-ht (make-hasheq))
  (define (viable? doc [single-line? (not multi-line?)])
    (let loop ([doc doc] [single-line? single-line?])
      (define (memoize r)
        (hash-set! (if single-line? v-s-ht v-ht) doc (if r 'yes 'no))
        r)
      (define (viable? doc)
        (loop doc single-line?))
      (cond
        [(string? doc) #t]
        [(bytes? doc) #t]
        [(eq? doc 'nl)
         (not single-line?)]
        [(hash-ref (if single-line? v-s-ht v-ht) doc #f)
         => (lambda (r) (eq? r 'yes))]
        [(not (and (pair? doc) (list? doc)))
         (error 'render-pretty "bad format ~v" doc)]
        [(eq? (car doc) 'seq)
         (memoize
          (for/and ([doc (in-list (cdr doc))])
            (viable? doc)))]
        [(and (eq? (car doc) 'nest)
              (= (length doc) 3))
         (memoize
          (viable? (caddr doc)))]
        [(and (eq? (car doc) 'align)
              (= (length doc) 2))
         (memoize
          (viable? (cadr doc)))]
        [(and (eq? (car doc) 'or)
              (= (length doc) 3))
         (memoize
          (or (viable? (cadr doc))
              (viable? (caddr doc))))]
        [(and (eq? (car doc) 'flat)
              (= (length doc) 2))
         (memoize
          (loop (cadr doc) #t))]
        [else
         (error 'render-pretty "bad format: ~v" doc)])))

  (unless (viable? doc)
    (error 'render-pretty "no way to render document: ~v" doc))
  
  (let loop ([doc doc] [col col] [indent indent] [single-line? (not multi-line?)])
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
         (loop doc col indent single-line?))]
      [(and (eq? (car doc) 'nest)
            (= (length doc) 3))
       (loop (caddr doc) col (+ indent (cadr doc)) single-line?)]
      [(and (eq? (car doc) 'align)
            (= (length doc) 2))
       (loop (cadr doc) col col single-line?)]
      [(and (eq? (car doc) 'or)
            (= (length doc) 3))
       (cond
         [(not (viable? (cadr doc) single-line?))
          (loop (caddr doc) col indent single-line?)]
         [(not (viable? (caddr doc) single-line?))
          (loop (cadr doc) col indent single-line?)]
         [multi-line?
          (loop (caddr doc) col indent single-line?)]
         [else
          (loop (cadr doc) col indent single-line?)])]
      [(and (eq? (car doc) 'flat)
            (= (length doc) 2))
       ;; must be viable, or we wouldn't get here
       (loop (cadr doc) col indent #t)]
      [else
       (error 'render-pretty "bad format: ~v" doc)]))

  (void))
