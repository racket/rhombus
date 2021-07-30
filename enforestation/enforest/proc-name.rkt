#lang racket/base

(provide proc-name)

(define (proc-name proc)
  (define s (object-name proc))
  (if (symbol? s)
      s
      'transformer-procedure))
