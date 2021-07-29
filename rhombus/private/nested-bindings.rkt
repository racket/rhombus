#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "binding.rkt")

(provide nested-bindings)

(define-syntax (nested-bindings stx)
  (syntax-parse stx
    [(_ who try-next failure post-defn body) #'(let () post-defn body)]
    [(_ who try-next failure post-defn (arg-id arg::binding-form arg-pat) . tail)
     #'(let-values ([(match? . arg.var-ids) (arg.check-proc-expr arg-id)])
         (if match?
             (nested-bindings
              who
              try-next failure
              (begin post-defn arg.post-defn)
              . tail)
             (if try-next
                 (try-next)
                 (failure 'who arg-id 'arg-pat))))]))
