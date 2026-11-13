#lang racket/base
(require "realm.rkt")

(provide raise-binding-failure
         raise-bindings-failure)

(define (raise-binding-failure who what val annotation-str . extra)
  (apply raise-arguments-error*
         who rhombus-realm
         (string-append what " does not satisfy annotation")
         what val
         (append extra
                 (list "annotation" (unquoted-printing-string
                                     (error-contract->adjusted-string
                                      annotation-str
                                      rhombus-realm))))))

(define (raise-bindings-failure who what vals annotation-str
                                #:msg [msg #f])
  (raise
   (exn:fail:contract
    (error-message->adjusted-string
     who
     rhombus-realm
     (apply string-append
            (or msg (string-append what " do not satisfy annotation"))
            "\n  " what "...:"
            (append
             (if (null? vals)
                 (list " [none]")
                 (for/list ([v (in-list vals)])
                   (string-append "\n   "
                                  ((error-value->string-handler)
                                   v
                                   (error-print-width)))))
             (if annotation-str
                 (list "\n  annotation: "
                       (error-contract->adjusted-string
                        annotation-str
                        rhombus-realm))
                 '())))
     rhombus-realm)
    (current-continuation-marks))))
