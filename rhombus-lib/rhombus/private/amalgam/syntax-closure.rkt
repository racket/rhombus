#lang racket/base
(require enforest/syntax-local
         enforest/transformer)
(provide call-syntax-closure)

(define (call-syntax-closure proc-id data fail-string)
  (define proc (syntax-local-value* proc-id (lambda (v)
                                              (and (procedure? v)
                                                   v))))
  (unless proc
    (raise-syntax-error #f
                        fail-string
                        proc-id))
  (syntax-local-introduce
   (call-as-transformer
    proc-id
    (list (syntax-local-introduce data))
    syntax-track-origin #f
    proc)))
