#lang racket/base
(require "realm.rkt")

(provide raise-bad-macro-result)

(define (raise-bad-macro-result who what form
                                #:syntax-for? [syntax-for? #t]
                                #:single-group? [single-group? #t])
  (raise
   (exn:fail:contract
    (error-message->adjusted-string
     who
     rhombus-realm
     (string-append
      "invalid macro result"
      "\n  expected: " (if syntax-for?
                           (string-append (if single-group?
                                              "single-group syntax object for "
                                              "syntax object for ")
                                          what)
                           what)
      "\n  received:" (let ([str ((error-value->string-handler)
                                  form
                                  (error-print-width))])
                        (if (regexp-match? #rx"\n" str)
                            (regexp-replace* "\n" (string-append "\n" str) "\n    ")
                            (string-append " " str))))
     rhombus-realm)
    (current-continuation-marks))))
