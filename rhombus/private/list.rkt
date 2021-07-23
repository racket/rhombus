#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         "composite.rkt"
         "binding.rkt")

(provide cons
         (for-space rhombus/binding cons))

(define-binding-syntax cons  
  (binding-transformer
   (make-composite-binding-transformer #'pair? (list #'car #'cdr))))
