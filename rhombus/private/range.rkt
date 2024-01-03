#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "expression.rkt"
         "static-info.rkt"
         "parse.rkt"
         (prefix-in rhombus-a: "arithmetic.rkt")
         "sequence-constructor-key.rkt"
         "treelist.rkt"
         (submod "list.rkt" for-listable))

(provide ..)

(define-syntax ..
  (expression-infix-operator
   (expr-quote ..)
   (list
    (cons (expr-quote rhombus-a:+) 'weaker)
    (cons (expr-quote rhombus-a:-) 'weaker)
    (cons (expr-quote rhombus-a:*) 'weaker)
    (cons (expr-quote rhombus-a:/) 'weaker))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(_)
        (values (wrap-as-static-sequence #`(in-naturals #,form1))
                #'())]
       [(_ . more)
        #:with (~var rhs (:infix-op+expression+tail #'..)) #'(group . more)
        (values (wrap-as-static-sequence #`(in-listable-range #,form1 rhs.parsed))
                #'rhs.tail)]))
   'none))

(define-for-syntax (wrap-as-static-sequence stx)
  (wrap-static-info stx #'#%sequence-constructor #'#t))

(struct listable-range (range)
  #:property prop:sequence (lambda (r) (listable-range-range r))
  #:property prop:Listable (vector (lambda (r)
                                     (for/treelist ([e (listable-range-range r)])
                                       e))))

(define-sequence-syntax in-listable-range
  (lambda () #'in-listable-range/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(d) (_  a b)]
       #`[(d) (in-range a b)]]
      [_ #f])))

(define (in-listable-range/proc a b)
  (listable-range (in-range a b)))
