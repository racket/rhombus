#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         "define-operator.rkt"
         "realm.rkt"
         "rhombus-primitive.rkt"
         "compare-key.rkt")

(provide (for-spaces (#f
                      rhombus/repet)

                     (rename-out [rhombus+ +]
                                 [rhombus- -]
                                 [rhombus* *]
                                 [rhombus/ /]
                                 [rhombus** **])
                     .<
                     .<=
                     .=
                     .!=
                     .>=
                     .>

                     div
                     mod
                     rem

                     !
                     &&
                     \|\|

                     ==
                     !=

                     ===
                     is_now
                     is_same_number_or_object))

(module+ static-infos
  (provide (for-syntax number-static-infos
                       real-static-infos
                       rational-static-infos
                       int-static-infos
                       flonum-static-infos)))

(module+ precedence
  (provide (for-syntax comparison-precedences)))

(define-for-syntax number-static-infos
  ;; comparison actually requires real numbers, but we want to
  ;; propagate a comparsion operation from things like `+`, and
  ;; so it's simplest (and good enough in practice) to overapproximate
  ;; by pointing all numbers to `>`, etc.
  #'((#%compare ((< <)
                 (<= <=)
                 (= =)
                 (!= number!=?)
                 (>= >=)
                 (> >)))))

(define-for-syntax real-static-infos
  number-static-infos)
(define-for-syntax rational-static-infos
  real-static-infos)
(define-for-syntax int-static-infos
  real-static-infos)
(define-for-syntax flonum-static-infos
  real-static-infos)

(set-primitive-contract! 'number? "Number")

(define-infix rhombus+ +
  #:weaker-than (rhombus** rhombus* rhombus/ div mod rem)
  #:same-as (rhombus-)
  #:static-infos #,number-static-infos)

(define-values-for-syntax (minus-expr-prefix minus-repet-prefix)
  (prefix rhombus- - #:weaker-than (rhombus** rhombus* rhombus/ div mod rem)
          #:static-infos #,number-static-infos))
(define-values-for-syntax (minus-expr-infix minus-repet-infix)
  (infix rhombus- - #:weaker-than (rhombus** rhombus* rhombus/ div mod rem)
         #:static-infos #,number-static-infos))

(define-syntax rhombus-
  (expression-prefix+infix-operator
   minus-expr-prefix
   minus-expr-infix))

(define-repetition-syntax rhombus-
  (repetition-prefix+infix-operator
   minus-repet-prefix
   minus-repet-infix))

(define-infix rhombus* *
  #:weaker-than (rhombus**)
  #:same-as (rhombus/)
  #:static-infos #,number-static-infos)

(define-infix rhombus/ /
  #:weaker-than (rhombus**)
  #:static-infos #,number-static-infos)

(define-infix rhombus** expt
  #:associate 'right
  #:static-infos #,number-static-infos)

(define-infix div quotient
  #:weaker-than (rhombus**)
  #:static-infos #,real-static-infos)
(define-infix mod modulo
  #:weaker-than (rhombus**)
  #:static-infos #,real-static-infos)
(define-infix rem remainder
  #:weaker-than (rhombus**)
  #:static-infos #,real-static-infos)

(define-prefix ! not
  #:stronger-than (&& \|\|))

(define-infix && and
  #:weaker-than (rhombus+ rhombus- rhombus* rhombus/ mod div rem rhombus**)
  #:stronger-than (\|\|))

(define-infix \|\| or
  #:weaker-than (rhombus+ rhombus- rhombus* rhombus/ mod div rem rhombus**))

(define-for-syntax comparison-precedences
  (lambda ()
    `((,(expr-quote rhombus+) . weaker)
      (,(expr-quote rhombus-) . weaker)
      (,(expr-quote rhombus*) . weaker)
      (,(expr-quote rhombus/) . weaker)
      (,(expr-quote mod) . weaker)
      (,(expr-quote div) . weaker)
      (,(expr-quote rem) . weaker)
      (,(expr-quote rhombus**) . weaker)
      (,(expr-quote .>) . same)
      (,(expr-quote .>=) . same)
      (,(expr-quote .=) . same)
      (,(expr-quote .!=) . same)
      (,(expr-quote .<) . same)
      (,(expr-quote .<=) . same)
      (,(expr-quote \|\|) . stronger)
      (,(expr-quote &&) . stronger))))

(define-syntax-rule (define-comp-infix name racket-name)
  (define-infix name racket-name
    #:precedences comparison-precedences
    #:associate 'none))

(set-primitive-who! '= '.=)

(define (number!=? a b)
  (if (and (number? a) (number? b))
      (not (= a b))
      (raise-argument-error* '!= rhombus-realm "Number" (if (number? a) b a))))

(define-comp-infix .< <)
(define-comp-infix .<= <=)
(define-comp-infix .= =)
(define-comp-infix .!= number!=?)
(define-comp-infix .>= >=)
(define-comp-infix .> >)

(define-syntax-rule (define-eql-infix name racket-name)
  (define-infix name racket-name
    #:weaker-than (rhombus+ rhombus- rhombus* rhombus/ mod div rem rhombus**)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(define-eql-infix == equal-always?)
(define-eql-infix != not-equal-always?)
(define-eql-infix === eq?)
(define-eql-infix is_now equal?)
(define-eql-infix is_same_number_or_object eqv?)

(define (not-equal-always? a b) (not (equal-always? a b)))
