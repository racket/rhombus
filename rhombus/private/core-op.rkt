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
  (define-syntax (prefix-operator stx)
    (syntax-parse stx
      [(_ name:identifier prim:identifier
          (~optional (~seq #:weaker-than (weaker-op ...))
                     #:defaults ([(weaker-op 1) '()]))
          (~optional (~seq #:same-as (same-op ...))
                     #:defaults ([(same-op 1) '()]))
          (~optional (~seq #:stronger-than (stronger-op ...))
                     #:defaults ([(stronger-op 1) '()])))
       #`(rhombus-prefix-operator (quote-syntax name)
                                  (list (cons (quote-syntax weaker-op)
                                              'weaker)
                                        ...
                                        (cons (quote-syntax same-op)
                                              'same)
                                        ...
                                        (cons (quote-syntax stronger-op)
                                              'stronger)
                                        ...)
                                  (lambda (form stx)
                                    (datum->syntax (quote-syntax here)
                                                   (list 'prim form)
                                                   (span-srcloc stx form)
                                                   stx)))]))

  (define-syntax (infix-operator stx)
    (syntax-parse stx
      [(_ name:identifier prim:identifier
          (~optional (~seq #:weaker-than (weaker-op ...))
                     #:defaults ([(weaker-op 1) '()]))
          (~optional (~seq #:same-as (same-op ...))
                     #:defaults ([(same-op 1) '()]))
          (~optional (~seq #:stronger-than (stronger-op ...))
                     #:defaults ([(stronger-op 1) '()]))
          (~optional (~seq #:associate assoc)
                     #:defaults ([assoc #''left])))
       #`(rhombus-infix-operator (quote-syntax name)
                                 (list (cons (quote-syntax weaker-op)
                                             'weaker)
                                       ...
                                       (cons (quote-syntax same-op)
                                             'same)
                                       ...
                                       (cons (quote-syntax stronger-op)
                                             'stronger)
                                       ...)
                                 assoc
                                 (lambda (form1 form2 stx)
                                   (datum->syntax (quote-syntax here)
                                                  (list 'prim form1 form2)
                                                  (span-srcloc form1 form2)
                                                  stx)))]))

  (struct rhombus-prefix+infix-operator (prefix infix)
    #:property prop:rhombus-prefix-operator (lambda (self) (rhombus-prefix+infix-operator-prefix self))
    #:property prop:rhombus-infix-operator (lambda (self) (rhombus-prefix+infix-operator-infix self))))

(define-syntax (define-infix-operator stx)
  (syntax-parse stx
    [(_ name spec ...)
     #'(define-syntax name (infix-operator name spec ...))]))

(define-syntax (define-prefix-operator stx)
  (syntax-parse stx
    [(_ name spec ...)
     #'(define-syntax name (prefix-operator name spec ...))]))

(define-infix-operator rhombus+ +
  #:weaker-than (rhombus* rhombus/)
  #:same-as (rhombus-))

(define-syntax rhombus-
  (rhombus-prefix+infix-operator
   (prefix-operator rhombus- - #:weaker-than (rhombus* rhombus/))
   (infix-operator rhombus- - #:weaker-than (rhombus* rhombus/))))

(define-infix-operator rhombus* *
  #:same-as (rhombus/))

(define-infix-operator rhombus/ /)

(define-prefix-operator ! not
  #:stronger-than (&& \|\|))

(define-infix-operator && and
  #:stronger-than (\|\|))

(define-infix-operator \|\| or)

(define-infix-operator rhombus< <
  #:same-as (rhombus> rhombus>= rhombus== rhombus<=)
  #:stronger-than (\|\| &&))
(define-infix-operator rhombus<= <=
  #:same-as (rhombus> rhombus>= rhombus==)
  #:stronger-than (\|\| &&))
(define-infix-operator rhombus== =
  #:same-as (rhombus> rhombus>=)
  #:stronger-than (\|\| &&))
(define-infix-operator rhombus>= >=
  #:same-as (rhombus>)
  #:stronger-than (\|\| &&))
(define-infix-operator rhombus> >
  #:stronger-than (\|\| &&))
