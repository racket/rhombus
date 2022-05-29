#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "expression.rkt"
         "define-operator.rkt"
         (only-in "dot.rkt"
                  |.|))

(provide (rename-out [rhombus+ +]
                     [rhombus- -]
                     [rhombus* *]
                     [rhombus/ /]
                     [rhombus< <]
                     [rhombus<= <=]
                     [rhombus>= >=]
                     [rhombus> >])
         .=
         sqrt cos sin tan log exp expt
         floor ceiling round

         !
         &&
         \|\|

         ==
         !=

         ===)

(define-infix rhombus+ +
  #:weaker-than (rhombus* rhombus/)
  #:same-as (rhombus-))

(define-syntax rhombus-
  (expression-prefix+infix-operator
   (prefix rhombus- - #:weaker-than (rhombus* rhombus/))
   (infix rhombus- - #:weaker-than (rhombus* rhombus/))))

(define-infix rhombus* *
  #:same-on-left-as (rhombus/))

(define-infix rhombus/ /)

(define-prefix ! not
  #:stronger-than (&& \|\|))

(define-infix && and
  #:weaker-than (rhombus+ rhombus- rhombus* rhombus/)
  #:stronger-than (\|\|))

(define-infix \|\| or
  #:weaker-than (rhombus+ rhombus- rhombus* rhombus/))

(define-syntax-rule (define-comp-infix name racket-name)
  (define-infix name racket-name
    #:weaker-than (rhombus+ rhombus- rhombus* rhombus/)
    #:same-as (rhombus> rhombus>= .= rhombus<=)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(define-comp-infix rhombus< <)
(define-comp-infix rhombus<= <=)
(define-comp-infix .= =)
(define-comp-infix rhombus>= >=)
(define-comp-infix rhombus> >)

(define-syntax-rule (define-eql-infix name racket-name)
  (define-infix name racket-name
    #:weaker-than (rhombus+ rhombus- rhombus* rhombus/ |.|)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(define-eql-infix == equal-always?)
(define-eql-infix != not-equal-always?)
(define-eql-infix === eq?)

(define (not-equal-always? a b) (not (equal-always? a b)))
