#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "static-info.rkt"
         "parse.rkt"
         (prefix-in rhombus-a: "arithmetic.rkt")
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "treelist.rkt"
         (submod "list.rkt" for-listable)
         "realm.rkt"
         "number.rkt")

(provide ..
         ..=)

(define-syntax ..
  (expression-infix-operator
   (expr-quote ..)
   `((,(expr-quote rhombus-a:+) . weaker)
     (,(expr-quote rhombus-a:-) . weaker)
     (,(expr-quote rhombus-a:*) . weaker)
     (,(expr-quote rhombus-a:/) . weaker))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(_)
        (values (wrap-as-static-sequence #`(in-infinite-range #,form1))
                #'())]
       [(_ . more)
        #:with (~var rhs (:infix-op+expression+tail #'..)) #'(group . more)
        (values (wrap-as-static-sequence #`(in-listable-range #,form1 rhs.parsed))
                #'rhs.tail)]))
   'none))

(define-syntax ..=
  (expression-infix-operator
   (expr-quote ..=)
   `((,(expr-quote rhombus-a:+) . weaker)
     (,(expr-quote rhombus-a:-) . weaker)
     (,(expr-quote rhombus-a:*) . weaker)
     (,(expr-quote rhombus-a:/) . weaker))
   'automatic
   (lambda (form1 form2 stx)
     (wrap-as-static-sequence #`(in-listable-inclusive-range #,form1 #,form2)))
   'none))

(define-for-syntax (wrap-as-static-sequence stx)
  (wrap-static-info (wrap-static-info stx #'#%sequence-constructor #'#t)
                    #'#%sequence-element int-static-infos))

(struct listable-range (range)
  #:property prop:sequence (lambda (r) (listable-range-range r))
  #:property prop:Listable (vector (lambda (r)
                                     (for/treelist ([e (listable-range-range r)])
                                       e))))

(define-syntax (define-listable-range stx)
  (syntax-parse stx
    [(_ name in-range)
     #'(begin
         (define-sequence-syntax name
           (lambda () #'proc)
           (lambda (stx)
             (syntax-parse stx
               [[(d) (_ a b)]
                #'[(d) (in-range a b)]]
               [_ #f])))
         (define proc
           (let ([name (lambda (a b)
                         (listable-range (in-range a b)))])
             name)))]))

(define-listable-range in-listable-range in-range)

(define-listable-range in-listable-inclusive-range in-inclusive-range)

(define (check-integer who int)
  (unless (exact-integer? int)
    (raise-argument-error* who rhombus-realm "Int" int)))

(define-sequence-syntax in-infinite-range
  (lambda () #'in-infinite-range/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ start-expr)]
       #'[(id)
          (:do-in
           ([(start) start-expr])
           (unless (variable-reference-from-unsafe? (#%variable-reference))
             (check-integer 'in-infinite-range start))
           ([pos start])
           #t
           ([(id) pos])
           #t
           #t
           ((+ pos 1)))]]
      [_ #f])))

(define (in-infinite-range/proc start)
  (check-integer 'in-infinite-range start)
  (infinite-range start))

(struct infinite-range (start)
  #:property prop:sequence (lambda (r)
                             (make-do-sequence
                              (lambda ()
                                (values
                                 values
                                 #f
                                 add1
                                 (infinite-range-start r)
                                 #f
                                 #f
                                 #f)))))
