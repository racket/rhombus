#lang racket/base
(provide format-id)

(define (format-id str ctx)
  (datum->syntax ctx
                 (string->symbol (format str (syntax-e ctx)))
                 ctx))

