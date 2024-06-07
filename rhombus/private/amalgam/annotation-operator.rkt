#lang racket/base
(require (for-syntax racket/base
                     enforest/operator
                     enforest/property
                     "misuse.rkt"))

(begin-for-syntax
  (provide (property-out annotation-prefix-operator)
           (property-out annotation-infix-operator))

  (property annotation-prefix-operator prefix-operator
            #:property prop:procedure (make-raise-misuse "annotation"))
  (property annotation-infix-operator infix-operator
            #:property prop:procedure (make-raise-misuse "annotation")))
