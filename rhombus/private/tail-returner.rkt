#lang racket/base
(require enforest/proc-name
         "realm.rkt")

(provide tail-returner)

(define-syntax-rule (tail-returner proc e)
  (call-with-values
   (lambda () e)
   (case-lambda
     [(form new-tail) (values form new-tail)]
     [(form) (values form #'(group))]
     [args (wrong-result proc args)])))

(define (wrong-result proc args)
  (apply raise-result-arity-error* (proc-name proc) rhombus-realm 1 #f args))

