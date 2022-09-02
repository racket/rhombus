#lang racket/base
(require scribble/core
         "defining-element.rkt")

(provide make-line-shape
         line-shape-step
         line-shape-newline
         line-shape-apply)

(struct elem-shape (len style)
  #:transparent)

(struct line-shape (first-line
                    prev-line
                    rev-line)
  #:transparent)

(define (make-line-shape)
  (line-shape #f '() '()))

;; record shape of `e` in rev-line while consumig `prev-line`
(define (line-shape-step ls e)
  (define es (element-shape e))
  (if (eqv? 0 (elem-shape-len es))
      ls
      (struct-copy line-shape ls
                   [rev-line (cons es (line-shape-rev-line ls))]
                   [prev-line (consume (line-shape-prev-line ls) (elem-shape-len es))])))

(define (consume prev-line el)
  (cond
    [(null? prev-line) '()]
    [else
     (define fl (elem-shape-len (car prev-line)))
     (cond
       [(el . = . fl)
        (cdr prev-line)]
       [(el . < . fl)
        (cons (struct-copy elem-shape (car prev-line)
                           [len (- fl el)])
              (cdr prev-line))]
       [else
        (consume (cdr prev-line) (- el fl))])]))
         
(define (line-shape-newline ls)
  (cond
    [(line-shape-first-line ls)
     => (lambda (fl)
          (struct-copy line-shape ls
                       [prev-line fl]
                       [rev-line '()]))]
    [else
     (define fl (simplify (reverse (line-shape-rev-line ls))))
     (struct-copy line-shape ls
                  [first-line fl]
                  [prev-line fl]
                  [rev-line '()])]))

(define (line-shape-apply ls len)
  (define pl (line-shape-prev-line ls))
  (cond
    [(null? pl) (values len #f)]
    [else
     (define es (car pl))
     (values (min (elem-shape-len es) len)
             (elem-shape-style es))]))

(define (element-shape e)
  (elem-shape (content-width e)
              (let loop ([e e])
                (cond
                  [(pair? e) (loop (car e))]
                  [(defining-element? e) 'target]
                  [(element? e) (loop (element-content e))]
                  [else #f]))))

(define (simplify l)
  (cond
    [(null? l) l]
    [(null? (cdr l)) l]
    [(eq? (elem-shape-style (car l))
          (elem-shape-style (cadr l)))
     (simplify (cons (elem-shape (+ (elem-shape-len (car l))
                                    (elem-shape-len (cadr l)))
                                 (elem-shape-style (car l)))
                     (cddr l)))]
    [else
     (cons (car l) (simplify (cdr l)))]))
