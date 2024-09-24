#lang racket/base

(provide make-line-shape
         line-shape-step
         line-shape-step-nonws
         line-shape-newline
         line-shape-apply)

(struct elem-shape (len orig-len size style)
  #:transparent)

(struct line-shape (first-line
                    first-line-len
                    prev-line
                    rev-line
                    rev-line-len)
  #:transparent)

(define (make-line-shape)
  (line-shape #f 0 '() '() 0))

;; record shape of `e` in rev-line while consuming `prev-line`
(define (line-shape-step ls e e-len e-style get-element-shape)
  (define es (let-values ([(size style) (get-element-shape e e-len e-style)])
               (elem-shape e-len e-len size style)))
  (if (eqv? 0 (elem-shape-len es))
      ls
      (struct-copy line-shape ls
                   [rev-line (cons es (line-shape-rev-line ls))]
                   [rev-line-len (+ (elem-shape-len es) (line-shape-rev-line-len ls))]
                   [prev-line (consume (line-shape-prev-line ls) (elem-shape-orig-len es))])))

;; like `line-shape-step`, but if `e` is before the end of
;; the first line, then shrink the first line, because any relevant
;; indentation is relative to this line
(define (line-shape-step-nonws ls e e-len get-element-shape)
  (define len (line-shape-rev-line-len ls))
  (cond
    [(len . < . (line-shape-first-line-len ls))
     (line-shape-step (struct-copy line-shape ls
                                   [first-line (truncate-line (line-shape-first-line ls) len)]
                                   [first-line-len len]
                                   [prev-line null])
                      e
                      e-len
                      #f
                      get-element-shape)]
    [else
     (line-shape-step ls e e-len #f get-element-shape)]))

(define (consume prev-line e-len)
  (cond
    [(null? prev-line) '()]
    [else
     (define fl (elem-shape-len (car prev-line)))
     (cond
       [(e-len . = . fl)
        (cdr prev-line)]
       [(e-len . < . fl)
        (cons (struct-copy elem-shape (car prev-line)
                           [len (- fl e-len)])
              (cdr prev-line))]
       [else
        (consume (cdr prev-line) (- e-len fl))])]))
         
(define (line-shape-newline ls)
  (cond
    [(and (line-shape-first-line ls)
          ((line-shape-first-line-len ls) . > . (line-shape-rev-line-len ls)))
     (struct-copy line-shape ls
                  [prev-line (line-shape-first-line ls)]
                  [rev-line '()]
                  [rev-line-len 0])]
    [else
     (define fl (simplify (reverse (line-shape-rev-line ls))))
     (struct-copy line-shape ls
                  [first-line fl]
                  [first-line-len (line-shape-rev-line-len ls)]
                  [prev-line fl]
                  [rev-line '()]
                  [rev-line-len 0])]))

(define (line-shape-apply ls len)
  (define pl (line-shape-prev-line ls))
  (cond
    [(null? pl) (values len 0 len #f #f)]
    [else
     (define es (car pl))
     (values (min (elem-shape-len es) len)
             (- (elem-shape-orig-len es) (elem-shape-len es))
             (elem-shape-orig-len es)
             (elem-shape-size es)
             (elem-shape-style es))]))

(define (simplify l)
  (cond
    [(null? l) l]
    [(null? (cdr l)) l]
    [(eq? (elem-shape-style (car l))
          (elem-shape-style (cadr l)))
     (simplify (cons (elem-shape (+ (elem-shape-len (car l))
                                    (elem-shape-len (cadr l)))
                                 (+ (elem-shape-orig-len (car l))
                                    (elem-shape-orig-len (cadr l)))
                                 (+ (elem-shape-size (car l))
                                    (elem-shape-size (cadr l)))
                                 (elem-shape-style (car l)))
                     (cddr l)))]
    [else
     (cons (car l) (simplify (cdr l)))]))

(define (truncate-line elems len)
  (cond
    [(null? elems) null]
    [(= len 0) null]
    [(len . >= . (elem-shape-len (car elems)))
     (cons (car elems) (truncate-line (cdr elems) (- len (elem-shape-len (car elems)))))]
    [else
     (list (elem-shape len
                       (elem-shape-orig-len (car elems))
                       (elem-shape-size (car elems))
                       (elem-shape-style (car elems))))]))
