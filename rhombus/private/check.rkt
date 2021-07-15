#lang racket/base
(require syntax/stx
         syntax/parse
         "binding.rkt")

(provide check-expression-result
         check-binding-result
         check-transformer-result

         proc-name)

(define (check-expression-result form proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  form)

(define (check-binding-result form proc)
  (syntax-parse (if (syntax? form) form #'#f)
    [_::binding-form form]
    [_ (raise-result-error (proc-name proc) "binding-result?" form)]))

(define (check-transformer-result form tail proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  (unless (stx-list? tail) (raise-result-error (proc-name proc) "stx-list?" tail))
  (values form tail))

(define (proc-name proc)
  (define s (object-name proc))
  (if (symbol? s)
      s
      'transformer-procedure))
