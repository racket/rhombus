#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/unsafe/undefined
         "binding.rkt"
         "parse.rkt")

(provide nested-bindings)

(define-syntax (nested-bindings stx)
  (syntax-parse stx
    [(_ who try-next failure post-defn body) #'(let () post-defn body)]
    [(_ who try-next failure post-defn (arg-id arg::binding-form arg-pat arg-default) . tail)
     #:with arg-in (if (syntax-e #'arg-default)
                       #'(if (eq? arg-id unsafe-undefined)
                             (rhombus-expression arg-default)
                             arg-id)
                       #'arg-id)
     #'(let-values ([(match? . arg.var-ids) (arg.check-proc-expr arg-in)])
         (if match?
             (nested-bindings
              who
              try-next failure
              (begin post-defn arg.post-defn)
              . tail)
             (if try-next
                 (try-next)
                 (failure 'who arg-id 'arg-pat))))]))
