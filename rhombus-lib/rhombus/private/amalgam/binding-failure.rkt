#lang racket/base
(require "realm.rkt")

(provide raise-binding-failure)

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
