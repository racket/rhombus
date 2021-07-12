#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "op.rkt"))

(provide (rename-out [rhombus+ +]
                     [rhombus- -]
                     [rhombus* *]
                     [rhombus/ /]
                     [rhombus< <]
                     [rhombus<= <=]
                     [rhombus== ==]
                     [rhombus>= >=]
                     [rhombus> >])
         !
         &&
         \|\|)

(begin-for-syntax
  (require (for-syntax racket/base
                       syntax/parse))
  (define-syntax (binary-operator stx)
    (syntax-parse stx
      [(_ name:identifier prim:identifier
          (~optional (~seq #:less-than (greater-op ...))
                     #:defaults ([(greater-op 1) '()]))
          (~optional (~seq #:same-as (same-op ...))
                     #:defaults ([(same-op 1) '()]))
          (~optional (~seq #:greater-than (lesser-op ...))
                     #:defaults ([(lesser-op 1) '()]))
          (~optional (~seq #:associate assoc)
                     #:defaults ([assoc #''left])))
       #`(rhombus-binary-operator (quote-syntax name)
                                  (list (quote-syntax greater-op) ...)
                                  (list (quote-syntax same-op) ...)
                                  (list (quote-syntax lesser-op) ...)
                                  assoc
                                  (lambda (form1 form2 stx)
                                    (datum->syntax (quote-syntax here)
                                                   (list 'prim form1 form2)
                                                   (span-srcloc form1 form2)
                                                   stx)))]))

  (define-syntax (unary-operator stx)
    (syntax-parse stx
      [(_ name:identifier prim:identifier
          (~optional (~seq #:less-than (greater-op ...))
                     #:defaults ([(greater-op 1) '()]))
          (~optional (~seq #:same-as (same-op ...))
                     #:defaults ([(same-op 1) '()]))
          (~optional (~seq #:greater-than (lesser-op ...))
                     #:defaults ([(lesser-op 1) '()])))
       #`(rhombus-unary-operator (quote-syntax name)
                                 (list (quote-syntax greater-op) ...)
                                 (list (quote-syntax same-op) ...)
                                 (list (quote-syntax lesser-op) ...)
                                 (lambda (form stx)
                                   (datum->syntax (quote-syntax here)
                                                  (list 'prim form)
                                                  (span-srcloc stx form)
                                                  stx)))]))

  (struct rhombus-unary+binary-operator (unary binary)
    #:property prop:rhombus-unary-operator (lambda (self) (rhombus-unary+binary-operator-unary self))
    #:property prop:rhombus-binary-operator (lambda (self) (rhombus-unary+binary-operator-binary self))))

(define-syntax (define-binary-operator stx)
  (syntax-parse stx
    [(_ name spec ...)
     #'(define-syntax name (binary-operator name spec ...))]))

(define-syntax (define-unary-operator stx)
  (syntax-parse stx
    [(_ name spec ...)
     #'(define-syntax name (unary-operator name spec ...))]))

(define-binary-operator rhombus+ +
  #:less-than (rhombus* rhombus/)
  #:same-as (rhombus-))

(define-syntax rhombus-
  (rhombus-unary+binary-operator
   (unary-operator rhombus- - #:less-than (rhombus* rhombus/))
   (binary-operator rhombus- - #:less-than (rhombus* rhombus/))))

(define-binary-operator rhombus* *
  #:same-as (rhombus/))

(define-binary-operator rhombus/ /)

(define-unary-operator ! not
  #:greater-than (&& \|\|))

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
