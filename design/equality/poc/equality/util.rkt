#lang racket/base

(provide values->list
         measure-ms
         display-results)

(require racket/list
         qi)

(define-syntax-rule (values->list expr)
  (call-with-values (λ () expr)
                    list))

(define (measure-ms fn . args)
  (second (values->list (time-apply fn args))))

(define-flow display-results
  (~> △
      (-< _ (~> / round))
      (>< number->string)
      (string-append "Built-in = " _ "ms" " | "
                     " Key = " _ "ms" " | "
                     "Speedup = " _ "X")
      displayln))
