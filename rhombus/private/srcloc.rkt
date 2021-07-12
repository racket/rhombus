#lang racket/base

(provide span-srcloc)

(define (span-srcloc start end)
  (vector (syntax-source start)
          (syntax-line start)
          (syntax-column start)
          (syntax-position start)
          (let ([e (syntax-position end)]
                [s (syntax-position start)]
                [sp (syntax-span end)])
            (and s e sp
                 (+ (- e s) sp)))))
