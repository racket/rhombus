#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "expression.rkt")

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
         \|\|
         +$)

(begin-for-syntax
  (require (for-syntax racket/base
                       syntax/parse))
  (define-syntax (prefix stx)
    (syntax-parse stx
      [(_ name:identifier prim:identifier
          (~optional (~seq #:weaker-than (weaker-op ...))
                     #:defaults ([(weaker-op 1) '()]))
          (~optional (~seq #:same-as (same-op ...))
                     #:defaults ([(same-op 1) '()]))
          (~optional (~seq #:stronger-than (stronger-op ...))
                     #:defaults ([(stronger-op 1) '()])))
       #`(expression-prefix-operator (quote-syntax name)
                                     (list (cons (quote-syntax weaker-op)
                                                 'weaker)
                                           ...
                                           (cons (quote-syntax same-op)
                                                 'same)
                                           ...
                                           (cons (quote-syntax stronger-op)
                                                 'stronger)
                                           ...)
                                     'automatic
                                     (lambda (form stx)
                                       (datum->syntax (quote-syntax here)
                                                      (list (quote-syntax prim) form)
                                                      (span-srcloc stx form)
                                                      stx)))]))

  (define-syntax (infix stx)
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
       #`(expression-infix-operator (quote-syntax name)
                                    (list (cons (quote-syntax weaker-op)
                                                'weaker)
                                          ...
                                          (cons (quote-syntax same-op)
                                                'same)
                                          ...
                                          (cons (quote-syntax stronger-op)
                                                'stronger)
                                          ...)
                                    'automatic
                                    (lambda (form1 form2 stx)
                                      (datum->syntax (quote-syntax here)
                                                     (list (quote-syntax prim) form1 form2)
                                                     (span-srcloc form1 form2)
                                                     stx))
                                    assoc)])))

(define-syntax (define-infix stx)
  (syntax-parse stx
    [(_ name spec ...)
     #'(define-syntax name (infix name spec ...))]))

(define-syntax (define-prefix stx)
  (syntax-parse stx
    [(_ name spec ...)
     #'(define-syntax name (prefix name spec ...))]))

(define-infix rhombus+ +
  #:weaker-than (rhombus* rhombus/)
  #:same-as (rhombus-))

(define-syntax rhombus-
  (expression-prefix+infix-operator
   (prefix rhombus- - #:weaker-than (rhombus* rhombus/))
   (infix rhombus- - #:weaker-than (rhombus* rhombus/))))

(define-infix rhombus* *
  #:same-as (rhombus/))

(define-infix rhombus/ /)

(define-prefix ! not
  #:stronger-than (&& \|\|))

(define-infix && and
  #:stronger-than (\|\|))

(define-infix \|\| or)

(define-infix rhombus< <
  #:same-as (rhombus> rhombus>= rhombus== rhombus<=)
  #:stronger-than (\|\| &&))
(define-infix rhombus<= <=
  #:same-as (rhombus> rhombus>= rhombus==)
  #:stronger-than (\|\| &&))
(define-infix rhombus== =
  #:same-as (rhombus> rhombus>=)
  #:stronger-than (\|\| &&))
(define-infix rhombus>= >=
  #:same-as (rhombus>)
  #:stronger-than (\|\| &&))
(define-infix rhombus> >
  #:stronger-than (\|\| &&))

(define-infix +$ string-append)
