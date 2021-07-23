#lang racket/base

(provide span-srcloc
         relocate)

(define (span-srcloc start end)
  (vector (syntax-source start)
          (syntax-line start)
          (syntax-column start)
          (syntax-position start)
          (let ([e (syntax-position end)]
                [s (syntax-position start)]
                [sp (syntax-span end)])
            (and s e sp
                 (max 0 (+ (- e s) sp))))))

(define (relocate srcloc stx)
  (datum->syntax stx (syntax-e stx) srcloc stx))
