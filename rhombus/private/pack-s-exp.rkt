#lang racket/base
(require syntax/parse/pre
         "pack.rkt"
         "realm.rkt")

(provide pack-s-exp)

(define (pack-s-exp who orig-s)
  (let loop ([s orig-s])
    (cond
      [(syntax? s)
       (define stx (unpack-term s who #f))
       (syntax-parse stx
         #:datum-literals (parsed)
         [(parsed #:rhombus/expr e) #'e]
         [_ stx])]
      [(list? s)
       (for/list ([e (in-list s)])
         (loop e))]
      [else
       (raise-arguments-error* who rhombus-realm
                               "not a list nesting of syntax objects"
                               "value" orig-s)])))
