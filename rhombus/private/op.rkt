#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "parse.rkt")

(provide (rename-out [rhombus+ +]
                     [rhombus- -]
                     [rhombus* *]
                     [rhombus/ /]
                     [rhombus< <]
                     [rhombus<= <=]
                     [rhombus== ==]
                     [rhombus>= >=]
                     [rhombus> >])
         &&
         \|\|)

(define-syntax (define-binary-operator stx)
  (syntax-parse stx
    [(_ name prim
        (~optional (~seq #:less-than (greater-op ...))
                   #:defaults ([(greater-op 1) '()]))
        (~optional (~seq #:same-as (same-op ...))
                   #:defaults ([(same-op 1) '()]))
        (~optional (~seq #:greater-than (lesser-op ...))
                   #:defaults ([(lesser-op 1) '()]))
        (~optional (~seq #:associate assoc)
                   #:defaults ([assoc #''left])))
     #`(define-syntax name
         (rhombus-binary-operator (quote-syntax name)
                                  (lambda (form1 form2 stx)
                                    (datum->syntax (quote-syntax here)
                                                   (list 'prim form1 form2)
                                                   (span-srcloc form1 form2)
                                                   stx))
                                  (list (quote-syntax greater-op) ...)
                                  (list (quote-syntax same-op) ...)
                                  (list (quote-syntax lesser-op) ...)
                                  assoc))]))

(define-binary-operator rhombus+ +
  #:less-than (rhombus* rhombus/)
  #:same-as (rhombus-))

(define-binary-operator rhombus- -
  #:less-than (rhombus* rhombus/))

(define-binary-operator rhombus* *
  #:same-as (rhombus/))

(define-binary-operator rhombus/ /)

(define-binary-operator && and
  #:greater-than (\|\|))

(define-binary-operator \|\| or)

(define-binary-operator rhombus< <
  #:same-as (rhombus> rhombus>= rhombus== rhombus<=)
  #:greater-than (\|\| &&))
(define-binary-operator rhombus<= <=
  #:same-as (rhombus> rhombus>= rhombus==)
  #:greater-than (\|\| &&))
(define-binary-operator rhombus== =
  #:same-as (rhombus> rhombus>=)
  #:greater-than (\|\| &&))
(define-binary-operator rhombus>= >=
  #:same-as (rhombus>)
  #:greater-than (\|\| &&))
(define-binary-operator rhombus> >
  #:greater-than (\|\| &&))
