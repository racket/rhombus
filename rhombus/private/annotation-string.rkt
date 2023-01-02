#lang racket/base

(provide annotation-any-string
         annotation-string-from-pattern
         annotation-string-to-pattern
         annotation-string-and
         annotation-string-or
         annotation-string-convert-pair)

(define annotation-any-string "Any")
(define annotation-complex-any-string "(_ :: Any)")

(define (annotation-string-from-pattern p)
  (string-append "matching(" p ")"))

(define (annotation-string-to-pattern s)
  (cond
    [(regexp-match #rx"^matching[(](.*)[)]$" s)
     => (lambda (m) (cadr m))]
    [(string=? s annotation-any-string) "_"]
    [else
     (string-append "(_ :: " s ")")]))

(define (annotation-string-and a b)
  (cond
    [(equal? a annotation-any-string) b]
    [(equal? b annotation-any-string) a]
    [else
     (string-append "and(" a ", " b ")")]))

(define (annotation-string-or a b)
  (cond
    [(equal? a annotation-any-string) a]
    [(equal? b annotation-any-string) b]
    [else
     (string-append "or(" a ", " b ")")]))

(define (annotation-string-convert-pair s)
  (define p (annotation-string-to-pattern s))
  (cond
    [(regexp-match #rx"^Pair[(](.*)[)]$" p)
     => (lambda (m)
          ;; s is comma-separated, but outside matching parentheses
          (define s (cadr m))
          (let loop ([i 0] [depth 0])
            (cond
              [(equal? i (string-length s)) s]
              [else
               (define ch (string-ref s i))
               (case ch
                 [(#\,)
                  (cond
                    [(zero? depth)
                     (define spaced? (and ((add1 i) . < . (string-length s))
                                          (char=? #\space (string-ref s (add1 i)))))
                     (string-append (simplify-any (substring s 0 i))
                                    ":"
                                    (if spaced? " " "")
                                    (simplify-any (substring s (+ i (if spaced? 2 1)))))]
                    [else
                     (loop (add1 i) depth)])]
                 [(#\( #\[ #\{)
                  (loop (add1 i) (add1 depth))]
                 [(#\) #\] #\})
                  (loop (add1 i) (sub1 depth))]
                 [else
                  (loop (add1 i) depth)])])))]
    [else s]))

(define (simplify-any s)
  (if (string=? annotation-complex-any-string s)
      "_"
      s))
