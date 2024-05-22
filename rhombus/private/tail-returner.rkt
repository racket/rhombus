#lang racket/base
(require enforest/proc-name
         "realm.rkt")

(provide tail-returner)

(define-syntax tail-returner
  (syntax-rules ()
   [(_ #:empty-tail empty-tail proc e)
    (call-with-values
     (lambda () e)
     (case-lambda
       [(form new-tail) (values form new-tail)]
       [(form) (values form empty-tail)]
       [args (wrong-result proc args)]))]
   [(_ proc e)
    (tail-returner #:empty-tail #'(group) proc e)]))

(define (wrong-result proc args)
  (apply raise-result-arity-error* (proc-name proc) rhombus-realm 1 #f args))

