#lang racket/base
(require syntax/parse)

(provide infer-name)

(define (infer-name var-ids)
  (syntax-parse var-ids
    [(id) #'id]
    [_ (car (generate-temporaries (list #'rhs)))]))
