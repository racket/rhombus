#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         "define-operator.rkt"
         (only-in "dot.rkt"
                  |.|)
         "rhombus-primitive.rkt")

(provide (for-spaces (#f
                      rhombus/repet)

                     (rename-out [rhombus+ +]
                                 [rhombus- -]
                                 [rhombus* *]
                                 [rhombus/ /]
                                 [rhombus** **]
                                 [rhombus< <]
                                 [rhombus<= <=]
                                 [rhombus>= >=]
                                 [rhombus> >])
                     .=

                     div
                     mod
                     rem

                     !
                     &&
                     \|\|

                     ==
                     !=

                     ===
                     is_now))

(set-primitive-contract! 'number? "Number")

(define-infix rhombus+ +
  #:weaker-than (rhombus** rhombus* rhombus/ div mod rem)
  #:same-as (rhombus-))

(define-values-for-syntax (minus-expr-prefix minus-repet-prefix)
  (prefix rhombus- - #:weaker-than (rhombus** rhombus* rhombus/ div mod rem)))
(define-values-for-syntax (minus-expr-infix minus-repet-infix)
  (infix rhombus- - #:weaker-than (rhombus** rhombus* rhombus/ div mod rem)))

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
  #:same-on-left-as (rhombus/ div mod rem))

(define-infix rhombus/ /
  #:weaker-than (rhombus**))

(define-infix rhombus** expt
  #:associate 'right)

(define-infix div quotient
  #:weaker-than (rhombus**))
(define-infix mod modulo
  #:weaker-than (rhombus**))
(define-infix rem remainder
  #:weaker-than (rhombus**))

(define-prefix ! not
  #:stronger-than (&& \|\|))

(define-infix && and
  #:weaker-than (rhombus+ rhombus- rhombus* rhombus/ mod div rem rhombus**)
  #:stronger-than (\|\|))

(define-infix \|\| or
  #:weaker-than (rhombus+ rhombus- rhombus* rhombus/ mod div rem rhombus**))

(define-syntax-rule (define-comp-infix name racket-name)
  (define-infix name racket-name
    #:weaker-than (rhombus+ rhombus- rhombus* rhombus/ mod div rem rhombus**)
    #:same-as (rhombus> rhombus>= .= rhombus<=)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(set-primitive-who! '= '.=)

(define-comp-infix rhombus< <)
(define-comp-infix rhombus<= <=)
(define-comp-infix .= =)
(define-comp-infix rhombus>= >=)
(define-comp-infix rhombus> >)

(define-syntax-rule (define-eql-infix name racket-name)
  (define-infix name racket-name
    #:weaker-than (rhombus+ rhombus- rhombus* rhombus/ mod div rem rhombus** |.|)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(define-eql-infix == equal-always?)
(define-eql-infix != not-equal-always?)
(define-eql-infix === eq?)
(define-eql-infix is_now equal?)

(define (not-equal-always? a b) (not (equal-always? a b)))
