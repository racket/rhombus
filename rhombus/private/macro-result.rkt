#lang racket/base

(provide raise-bad-macro-result)

(define (raise-bad-macro-result who what form
                                #:syntax-for? [syntax-for? #t])
  (raise
   (exn:fail:contract
    (format (string-append "~ainvalid macro result\n"
                           "  expected: ~a~a\n"
                           "  received: ~v")
            (if who
                (format "~a: " who)
                "")
            (if syntax-for?
                "syntax object for "
                "")
            what
            form)
    (current-continuation-marks))))
