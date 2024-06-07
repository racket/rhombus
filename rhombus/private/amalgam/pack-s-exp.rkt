#lang racket/base
(require syntax/parse/pre
         "pack.rkt"
         "realm.rkt"
         "to-list.rkt")

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
      [(to-list #f s)
       => (lambda (es)
            (for/list ([e (in-list es)])
              (loop e)))]
      [else
       (raise-arguments-error* who rhombus-realm
                               "not a listable nesting of syntax objects"
                               "value" orig-s)])))
