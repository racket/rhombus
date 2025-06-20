#lang racket/base
(require "realm.rkt")

(provide raise-srcloc-error)

(struct exn:fail:contract:srcloc exn:fail:contract (srclocs)
  #:property prop:exn:srclocs (lambda (exn) (exn:fail:contract:srcloc-srclocs exn)))

(define (raise-srcloc-error who msg loc)
  (raise
   (exn:fail:contract:srcloc
    (error-message->adjusted-string
     who
     rhombus-realm
     msg
     rhombus-realm)
    (current-continuation-marks)
    (if loc
        (list loc)
        null))))
