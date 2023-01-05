#lang racket

(provide call_capturing_exn
         call_capturing_values
         does_contain_each)

(define (call_capturing_exn thunk)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (values #f (exn-message exn)))])
    (values (call-with-values thunk list) #f)))

(define (call_capturing_values thunk)
  (call-with-values thunk list))

(define (does_contain_each strs in-str)
  (for/and ([str (in-list strs)])
    (does_contain str in-str)))

(define (does_contain str in-str)
  (or (equal? str "")
      (let loop ([i 0])
        (cond
          [(< (- (string-length in-str) i)
              (string-length str))
           #f]
          [(and (eqv? (string-ref str 0)
                      (string-ref in-str i))
                (string=? str
                          (substring in-str i (+ i (string-length str)))))
           #t]
          [else
           (loop (add1 i))]))))
