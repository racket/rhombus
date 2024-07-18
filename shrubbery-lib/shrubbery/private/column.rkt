#lang racket/base


(provide count-graphemes

         column+
         column=?
         column<?
         column>?
         column<=>?)

;; represent a column in general as
;;   (list num-chars num-tabs ... num-chars)
;; where the start of the list is the rightmost set of chars/tabs,
;; always starting with a number of chars, always ending with a number
;; of chars, with 0s only potentially at the start and end, and
;; with a plain number as a shorthand for a list containing that number

;; add `c1` columns (on right) to base column offset `c2`
(define (column+ c1 c2)
  (cond
    [(and (number? c1) (number? c2))
     (+ c1 c2)]
    [(number? c1)
     (cons (+ c1 (car c2)) (cdr c2))]
    [else
     (let loop ([c1 c1])
       (cond
         [(null? c1) (if (number? c2)
                         (list c2)
                         c2)]
         [(null? (cdr c1)) (if (number? c2)
                               (list (+ (car c1) c2))
                               (cons (+ (car c1) (car c2)) (cdr c2)))]
         [else (list* (car c1) (cadr c1) (loop (cddr c1)))]))]))

(define (column=? c1 c2 #:incomparable [incomparable (lambda () #f)])
  (column<=>? = c1 c2 incomparable))

(define (column<? c1 c2 #:incomparable [incomparable (lambda () #f)])
  (column<=>? < c1 c2 incomparable))

(define (column>? c1 c2 #:incomparable [incomparable (lambda () #f)])
  (column<=>? > c1 c2 incomparable))

(define (column<=>? <=>? c1 c2 incomparable)
  (cond
    [(and (number? c1) (number? c2)) (<=>? c1 c2)]
    [(number? c1) (column<=>? <=>? (list c1) c2 incomparable)]
    [(number? c2) (column<=>? <=>? c1 (list c2) incomparable)]
    [else
     (let loop ([rev-c1 (reverse c1)]
                [rev-c2 (reverse c2)])
       (cond
         [(null? rev-c2) (if (null? rev-c1)
                             (<=>? 1 1)
                             (<=>? 1 0))]
         [(null? rev-c1) (if (null? rev-c2)
                             (<=>? 1 1)
                             (<=>? 0 1))]
         [else
          (if (= (car rev-c1) (car rev-c2))
              (loop (cdr rev-c1) (cdr rev-c2))
              (if (> (car rev-c1) (car rev-c2))
                  (if (null? (cdr rev-c2))
                      (<=>? 1 0)
                      ;; allow half a column up before a tab
                      (if (and (= (car rev-c1) (+ (car rev-c2) 0.5))
                               (null? (cdr rev-c1)))
                          #f
                          (incomparable)))
                  (if (null? (cdr rev-c1))
                      (<=>? 0 1)
                      ;; allow half a column up before a tab
                      (if (and (= (+ (car rev-c1) 0.5) (car rev-c2))
                               (null? (cdr rev-c2)))
                          #f
                          (incomparable)))))]))]))

(define (count-graphemes s [lines 0] [columns 0])
  (let loop ([i 0] [lines lines] [columns columns])
    (cond
      [(= i (string-length s)) (values lines columns)]
      [(char=? #\return (string-ref s i))
       (if (and ((add1 i) . < . (string-length s))
                (char=? #\newline (string-ref s (add1 i))))
           (loop (+ i 2) (add1 lines) 0)
           (loop (+ i 1) (add1 lines) 0))]
      [(char=? #\newline (string-ref s i))
       (loop (+ i 1) (add1 lines) 0)]
      [(char=? #\tab (string-ref s i))
       (loop (+ i 1) lines (column+ '(0 1 0) columns))]
      [else
       (define n (string-grapheme-span s i))
       (loop (+ i n) lines (column+ 1 columns))])))
