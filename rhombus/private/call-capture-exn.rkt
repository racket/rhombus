#lang racket/base

(provide call_capturing_exn
         call_capturing_values
         does_contain_each
         display_as_exn)

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

(struct exn:fail:test exn:fail (srcloc)
  #:property prop:exn:srclocs (lambda (e)
                                (list (exn:fail:test-srcloc e))))

(define (display_as_exn msg loc)
  (if loc
      ((error-display-handler) msg
                               (exn:fail:test msg
                                              (current-continuation-marks)
                                              loc))
      (displayln msg (current-error-port))))
