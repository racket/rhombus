#lang racket/base
(require syntax/parse/pre)

(provide infer-name)

(define (infer-name var-ids)
  (syntax-parse var-ids
    [(id) #'id]
    [_ (car (generate-temporaries (list #'rhs)))]))
