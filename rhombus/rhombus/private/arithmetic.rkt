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
         sqrt cos sin tan log exp expt
         floor ceiling round

         !
         &&
         \|\|

         &
         ===)

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
          (~optional (~seq #:same-on-right-as (same-on-right-op ...))
                     #:defaults ([(same-on-right-op 1) '()]))
          (~optional (~seq #:same-on-left-as (same-on-left-op ...))
                     #:defaults ([(same-on-left-op 1) '()]))
          (~optional (~seq #:stronger-than (stronger-op ...))
                     #:defaults ([(stronger-op 1) '()])))
       #`(expression-prefix-operator (quote-syntax name)
                                     (list (cons (quote-syntax weaker-op)
                                                 'weaker)
                                           ...
                                           (cons (quote-syntax same-op)
                                                 'same)
                                           ...
                                           (cons (quote-syntax same-on-right-op)
                                                 'same-on-right)
                                           ...
                                           (cons (quote-syntax same-on-left-op)
                                                 'same-on-left)
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
          (~optional (~seq #:same-on-left-as (same-on-left-op ...))
                     #:defaults ([(same-on-left-op 1) '()]))
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
                                          (cons (quote-syntax same-on-left-op)
                                                'same-on-left)
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
    #:same-as (rhombus> rhombus>= rhombus== rhombus<= ===)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(define-comp-infix rhombus< <)
(define-comp-infix rhombus<= <=)
(define-comp-infix rhombus== =)
(define-comp-infix rhombus>= >=)
(define-comp-infix rhombus> >)
(define-comp-infix === equal?)

(define-infix & append-as-strings
  #:stronger-than (===))

(define (append-as-strings a b)
  (string-append (if (string? a) a (format "~a" a))
                 (if (string? b) b (format "~a" b))))
