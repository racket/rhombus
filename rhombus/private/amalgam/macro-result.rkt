#lang racket/base
(require "realm.rkt")

(provide raise-bad-macro-result)

(define (raise-bad-macro-result who what form
                                #:syntax-for? [syntax-for? #t])
  (raise
   (exn:fail:contract
    (error-message->adjusted-string
     who
     rhombus-realm
     (string-append
      "invalid macro result"
      "\n  expected: " (if syntax-for?
                           (string-append "syntax object for " what)
                           what)
      "\n  received: " ((error-value->string-handler)
                        form
                        (error-print-width)))
     rhombus-realm)
    (current-continuation-marks))))
