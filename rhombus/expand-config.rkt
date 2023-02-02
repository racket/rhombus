#lang racket/base
(require "private/runtime-config.rkt")

(provide enter-parameterization
         exit-parameterization)

(define originals (make-parameter #f))

(define (enter-parameterization)
  (parameterize ([originals (for/list ([param (in-list parameters)])
                              (param))])
    (let loop ([parameters parameters])
      (cond
        [(null? parameters)
         (install-runtime-config!)
         (current-parameterization)]
        [else
         (parameterize ([(car parameters) ((car parameters))])
           (loop (cdr parameters)))]))))

(define (exit-parameterization)
  (let ([vals (originals)])
    (if vals
        (let loop ([parameters parameters] [vals vals])
          (cond
            [(null? parameters)
             (parameterize ([originals #f])
               (current-parameterization))]
            [else
             (parameterize ([(car parameters) (car vals)])
               (loop (cdr parameters) (cdr vals)))]))
        (current-parameterization))))
