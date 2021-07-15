#lang racket/base
(require syntax/stx
         "check.rkt"
         "property.rkt")

;; See "parse.rkt" for general information about operators and parsing.
;;
;; The compile-time nature of being a unary operator, binary
;; operator, etc., is represented using structure-type properties, so
;; a name can be bound to multiple of those (e.g., `-` is normally
;; both a unary and a binary operator). An binding cannot be both a
;; transformer binary operator and non-transformer binary operator,
;; however, because the transformer nature takes precedence.

;; An operator's predence is represented as a list of
;;
;;   (cons/c (or/c identifier? 'default)
;;           (or/c 'stronger 'same 'weaker))
;;
;; where the `car`s of the pairs should be distinct, and 'default
;; stands for every identifier not mentioned. The value 'stronger
;; means that the operator that has this list has a stronger
;; precedence than the one referenced by the identifier. An operator
;; is implicitly the 'same as itself (i.e., not covered by 'default).

(provide rhombus-operator?
         rhombus-operator-name
         rhombus-operator-precedences
         rhombus-operator-proc ; convention depends on category

         rhombus-prefix-operator
         rhombus-prefix-operator?

         rhombus-infix-operator
         rhombus-infix-operator?
         rhombus-infix-operator-assoc

         (property-out rhombus-prefix-expression-operator)
         (property-out rhombus-prefix-binding-operator)
         
         (property-out rhombus-infix-expression-operator)
         (property-out rhombus-infix-binding-operator)

         (property-out rhombus-prefix-expression-operator-transformer)
         (property-out rhombus-prefix-binding-operator-transformer)

         (property-out rhombus-infix-expression-operator-transformer)
         (property-out rhombus-infix-binding-operator-transformer))

(module+ for-parse
  (provide relative-precedence

           lookup-infix-implicit
           lookup-prefix-implicit

           apply-prefix-operator
           apply-infix-operator
           apply-prefix-operator-transformer
           apply-infix-operator-transformer

           expression-operator-ref
           binding-operator-ref))

(struct rhombus-operator (name precedences proc))
(struct rhombus-prefix-operator rhombus-operator ())
(struct rhombus-infix-operator rhombus-operator (assoc))

(property rhombus-prefix-expression-operator rhombus-prefix-operator)
(property rhombus-prefix-binding-operator rhombus-prefix-operator)

(property rhombus-infix-expression-operator rhombus-infix-operator)
(property rhombus-infix-binding-operator rhombus-infix-operator)

(property rhombus-prefix-expression-operator-transformer rhombus-prefix-operator
          #:super prop:rhombus-prefix-expression-operator)
(property rhombus-prefix-binding-operator-transformer rhombus-prefix-operator
          #:super prop:rhombus-prefix-binding-operator)

(property rhombus-infix-expression-operator-transformer rhombus-infix-operator
          #:super prop:rhombus-infix-expression-operator)
(property rhombus-infix-binding-operator-transformer rhombus-infix-operator
          #:super prop:rhombus-infix-binding-operator)

(define (expression-operator-ref v)
  (or (rhombus-prefix-expression-operator-ref v (lambda () #f))
      (rhombus-infix-expression-operator-ref v (lambda () #f))
      (error #f "identifier is not mapped to an expression operator: ~e" v)))

(define (binding-operator-ref v)
  (or (rhombus-prefix-binding-operator-ref v (lambda () #f))
      (rhombus-infix-binding-operator-ref v (lambda () #f))
      (error #f "identifier is not mapped to  binding operator: ~e" v)))

;; All helper functions from here on expect unwrapped operators (i.e.,
;; an accessor like `rhombus-prefix-expression-operator-ref` has already
;; been applied).

;; returns: 'stronger, 'weaker, 'same (no associativity), #f (not related)
(define (relative-precedence op other-op head)
  (define (find op-name this-op-name precs)
    (let loop ([precs precs] [default #f])
      (cond
        [(null? precs) (if (free-identifier=? op-name this-op-name)
                           'same
                           default)]
        [(eq? (caar precs) 'default) (loop (cdr precs) (cdar precs))]
        [(free-identifier=? op-name (caar precs)) (cdar precs)]
        [else (loop (cdr precs) default)])))
  (define (invert dir)
    (case dir
      [(stronger) 'weaker]
      [(weaker) 'stronger]
      [else dir]))
  (define op-name (rhombus-operator-name op))
  (define other-op-name (rhombus-operator-name other-op))
  (define dir1 (find other-op-name op-name (rhombus-operator-precedences op)))
  (define dir2 (invert (find op-name other-op-name (rhombus-operator-precedences other-op))))
  (define (raise-inconsistent how)
    (raise-syntax-error #f
                        (format
                         (string-append "inconsistent operator ~a declared\n"
                                        "  one operator: ~a\n"
                                        "  other operator: ~a")
                         how
                         (syntax-e (rhombus-operator-name op))
                         (syntax-e (rhombus-operator-name other-op)))
                        head))
  (when (and dir1 dir2 (not (eq? dir1 dir2)))
    (raise-inconsistent "precedence"))
  (define dir (or dir1 dir2
                  (and (free-identifier=? (rhombus-operator-name op)
                                          (rhombus-operator-name other-op))
                       'same)))
  (cond
    [(eq? 'same dir)
     (define op-a (rhombus-infix-operator-assoc op))
     (when (rhombus-infix-operator? other-op)
       (unless (eq? op-a (rhombus-infix-operator-assoc other-op))
         (raise-inconsistent "associativity")))
     (case op-a
       [(left) 'weaker]
       [(right) 'stronger]
       [else 'same])]
    [else dir]))

(define (lookup-prefix-implicit alone-name adj-context operator? operator-kind)
  (define op-stx (datum->syntax adj-context alone-name))
  (define v (syntax-local-value op-stx (lambda () #f)))
  (unless (operator? v)
    (raise-syntax-error #f
                        (format (string-append
                                 "misplaced expression;\n"
                                 " no infix operator is between this expression and the previous one,\n"
                                 " and `~a` is not bound as an implicit prefix ~a")
                                alone-name
                                operator-kind)
                        adj-context))
  (values v op-stx))

(define (lookup-infix-implicit adjacent-name adj-context operator? operator-kind)
  (define op-stx (datum->syntax adj-context adjacent-name))
  (define v (syntax-local-value op-stx (lambda () #f)))
  (unless (operator? v)
    (raise-syntax-error #f
                        (format
                         (string-append
                          "misplaced expression;\n"
                          " no infix operator is between this expression and the previous one,\n"
                          " and `~a` is not bound as an implicit infix ~a")
                         adjacent-name
                         operator-kind)
                        adj-context))
  (values v op-stx))

(define (apply-prefix-operator op form stx checker)
  (define proc (rhombus-operator-proc op))
  (checker (proc form stx) proc))

(define (apply-infix-operator op form1 form2 stx checker)
  (define proc (rhombus-operator-proc op))
  (checker (proc form1 form2 stx) proc))

(define (apply-prefix-operator-transformer op tail checker)
  (define proc (rhombus-operator-proc op))
  (define-values (form new-tail) (proc tail))
  (check-transformer-result (checker form proc)
                            new-tail
                            proc))

(define (apply-infix-operator-transformer op form1 tail checker)
  (define proc (rhombus-operator-proc op))
  (define-values (form new-tail) (proc form1 tail))
  (check-transformer-result (checker form proc)
                            new-tail
                            proc))
