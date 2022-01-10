#lang racket/base
(require "realm.rkt")

(provide raise-contract-error)

(define (raise-contract-error who fmt . args)
  (raise
   (exn:fail:contract
    (error-message->adjusted-string
     who
     rhombus-realm
     (apply format fmt args)
     rhombus-realm)
    (current-continuation-marks))))

  
