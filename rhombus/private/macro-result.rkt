#lang racket/base

(provide raise-bad-macro-result)

(define (raise-bad-macro-result who what form)
  (raise
   (exn:fail:contract
    (format (string-append "~ainvalid macro result\n"
                           "  expected: syntax object for ~a\n"
                           "  received: ~v")
            (if who
                (format "~a: " who)
                "")
            what
            form)
    (current-continuation-marks))))
