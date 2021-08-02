#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "binding.rkt")

(provide values
         (for-space rhombus/binding values))

(define-binding-syntax values
  (binding-prefix-operator
   #'values
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            (string-append "not allowed as a pattern (except as a non-nested"
                                           " pattern by forms that specifically recognize it")
                            #'head)]))))
