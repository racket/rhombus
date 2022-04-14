#lang racket/base

(provide values->list
         measure-ms)

(require racket/list)

(define-syntax-rule (values->list expr)
  (call-with-values (λ () expr)
                    list))

(define (measure-ms fn . args)
  (second (values->list (time-apply fn args))))
