#lang racket/base
(require (only-in scribble/core
                  element
                  style))

(provide hspace-style
         keep-spaces)

(define hspace-style (style 'hspace null))

(define (keep-spaces str)
  (define (hspace n)
    (element hspace-style (make-string n #\space)))
  (cond
    [(regexp-match-positions #rx"^ +" str)
     => (lambda (m)
          (list (hspace (cdar m))
                (keep-spaces (substring str (cdar m)))))]
    [(regexp-match-positions #rx" +$" str)
     => (lambda (m)
          (list (keep-spaces (substring str 0 (caar m)))
                (hspace (- (cdar m) (caar m)))))]
    [(regexp-match-positions #rx"  +" str)
     => (lambda (m)
          (list (keep-spaces (substring str 0 (caar m)))
                (hspace (- (cdar m) (caar m)))
                (keep-spaces (substring str (cdar m)))))]
    [else str]))

