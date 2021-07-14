#lang racket/base
(require syntax/stx
         "check.rkt")

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

(provide rhombus-operator-name
         rhombus-operator-precedences

         rhombus-prefix-operator
         rhombus-prefix-operator?
         prop:rhombus-prefix-operator

         rhombus-prefix-pattern-operator
         rhombus-prefix-pattern-operator?
         prop:rhombus-prefix-pattern-operator

         rhombus-infix-operator-assoc
         rhombus-infix-operator-proc
         
         rhombus-infix-operator
         rhombus-infix-operator?
         prop:rhombus-infix-operator

         rhombus-infix-pattern-operator
         rhombus-infix-pattern-operator?
         prop:rhombus-infix-pattern-operator

         rhombus-prefix-operator-transformer
         rhombus-prefix-operator-transformer?
         prop:rhombus-prefix-operator-transformer

         rhombus-prefix-pattern-operator-transformer
         rhombus-prefix-pattern-operator-transformer?
         prop:rhombus-prefix-pattern-operator-transformer

         rhombus-infix-operator-transformer
         rhombus-infix-operator-transformer?
         prop:rhombus-infix-operator-transformer

         rhombus-infix-pattern-operator-transformer
         rhombus-infix-pattern-operator-transformer?
         prop:rhombus-infix-pattern-operator-transformer)

(module+ for-parse
  (provide relative-precedence

           lookup-infix-implicit
           lookup-prefix-implicit

           apply-prefix-operator
           apply-infix-operator
           apply-prefix-operator-transformer
           apply-infix-operator-transformer))

(define-values (prop:rhombus-prefix-operator rhombus-prefix-operator? rhombus-prefix-operator-ref)
  (make-struct-type-property 'rhombus-prefix-operator))
(define-values (prop:rhombus-prefix-pattern-operator rhombus-prefix-pattern-operator? rhombus-prefix-pattern-operator-ref)
  (make-struct-type-property 'rhombus-prefix-pattern-operator))
(define-values (prop:rhombus-infix-operator rhombus-infix-operator? rhombus-infix-operator-ref)
  (make-struct-type-property 'rhombus-infix-operator))
(define-values (prop:rhombus-infix-pattern-operator rhombus-infix-pattern-operator? rhombus-infix-pattern-operator-ref)
  (make-struct-type-property 'rhombus-infix-pattern-operator))

(define-values (prop:rhombus-prefix-operator-transformer rhombus-prefix-operator-transformer? rhombus-prefix-operator-transformer-ref)
  (make-struct-type-property 'rhombus-prefix-operator-transformer #f
                             (list (cons prop:rhombus-prefix-operator values))))
(define-values (prop:rhombus-prefix-pattern-operator-transformer rhombus-prefix-pattern-operator-transformer? rhombus-prefix-pattern-operator-transformer-ref)
  (make-struct-type-property 'rhombus-prefix-pattern-operator-transformer #f
                             (list (cons prop:rhombus-prefix-pattern-operator values))))
(define-values (prop:rhombus-infix-operator-transformer rhombus-infix-operator-transformer? rhombus-infix-operator-transformer-ref)
  (make-struct-type-property 'rhombus-infix-operator-transformer #f
                             (list (cons prop:rhombus-infix-operator values))))
(define-values (prop:rhombus-infix-pattern-operator-transformer rhombus-infix-pattern-operator-transformer? rhombus-infix-pattern-operator-transformer-ref)
  (make-struct-type-property 'rhombus-infix-pattern-operator-transformer #f
                             (list (cons prop:rhombus-infix-pattern-operator values))))

(struct operator (name precedences proc))

(struct prefix-operator operator ()
  #:property prop:rhombus-prefix-operator (lambda (self) self))
(struct prefix-pattern-operator operator ()
  #:property prop:rhombus-prefix-pattern-operator (lambda (self) self))

(struct infix-*operator operator (assoc))
(struct infix-operator infix-*operator ()
  #:property prop:rhombus-infix-operator (lambda (self) self))
(struct infix-pattern-operator infix-*operator ()
  #:property prop:rhombus-infix-pattern-operator (lambda (self) self))

;; convenience functions
(define (rhombus-prefix-operator name precedences proc)
  (prefix-operator name precedences proc))
(define (rhombus-prefix-pattern-operator name precedences proc)
  (prefix-pattern-operator name precedences proc))
(define (rhombus-infix-operator name precedences assoc proc)
  (infix-operator name precedences proc assoc))
(define (rhombus-infix-pattern-operator name precedences assoc proc)
  (infix-pattern-operator name precedences proc assoc))

(struct prefix-operator-transformer operator ()
  #:property prop:rhombus-prefix-operator-transformer (lambda (self) self))
(struct prefix-pattern-operator-transformer operator ()
  #:property prop:rhombus-prefix-pattern-operator-transformer (lambda (self) self))
(struct infix-operator-transformer infix-*operator ()
  #:property prop:rhombus-infix-operator-transformer (lambda (self) self))
(struct infix-pattern-operator-transformer infix-*operator ()
  #:property prop:rhombus-infix-pattern-operator-transformer (lambda (self) self))

(define (rhombus-prefix-operator-transformer name precedences proc)
  (prefix-operator-transformer name precedences proc))
(define (rhombus-prefix-pattern-operator-transformer name precedences proc)
  (prefix-pattern-operator-transformer name precedences proc))
(define (rhombus-infix-operator-transformer name precedences assoc proc)
  (infix-operator-transformer name precedences proc assoc))
(define (rhombus-infix-pattern-operator-transformer name precedences assoc proc)
  (infix-pattern-operator-transformer name precedences proc assoc))

(define (unwrap-operator o #:pattern? pattern?)
  (cond
    [(if pattern?
         (or (rhombus-prefix-pattern-operator-ref o #f)
             (rhombus-infix-pattern-operator-ref o #f))
         (or (rhombus-prefix-operator-ref o #f)
             (rhombus-infix-operator-ref o #f)))
     => (lambda (sel) (sel o))]
    [else (raise-argument-error 'operator-property (if pattern? "pattern-operator?" "operator?") o)]))

(define (rhombus-operator-name o #:pattern? pattern?)
  (operator-name (unwrap-operator o #:pattern? pattern?)))

(define (rhombus-operator-precedences o #:pattern? pattern?)
  (operator-precedences (unwrap-operator o #:pattern? pattern?)))

(define (unwrap-prefix-operator o #:pattern? pattern?)
  (cond
    [(if pattern?
         (rhombus-prefix-pattern-operator-ref o #f)
         (rhombus-prefix-operator-ref o #f))
     => (lambda (sel) (sel o))]
    [else (raise-argument-error 'operator-property (if pattern? "prefix-pattern-operator?" "prefix-operator?") o)]))

(define (rhombus-prefix-operator-proc o #:pattern? pattern?)
  (operator-proc (unwrap-prefix-operator o #:pattern? pattern?)))

(define (unwrap-infix-operator o #:pattern? pattern?)
  (cond
    [(if pattern?
         (rhombus-infix-pattern-operator-ref o #f)
         (rhombus-infix-operator-ref o #f))
     => (lambda (sel) (sel o))]
    [else (raise-argument-error 'operator-property (if pattern? "infix-pattern-operator?" "infix-operator?") o)]))

(define (rhombus-infix-operator-proc o #:pattern? pattern?)
  (operator-proc (unwrap-infix-operator o #:pattern? pattern?)))

(define (rhombus-infix-operator-assoc o #:pattern? pattern?)
  (infix-*operator-assoc (unwrap-infix-operator o #:pattern? pattern?)))

;; returns: 'stronger, 'weaker, 'same (no associativity), #f (not related)
(define (relative-precedence op other-op head #:pattern? pattern?)
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
  (define op-name (rhombus-operator-name op #:pattern? pattern?))
  (define other-op-name (rhombus-operator-name other-op #:pattern? pattern?))
  (define dir1 (find other-op-name op-name (rhombus-operator-precedences op #:pattern? pattern?)))
  (define dir2 (invert (find op-name other-op-name (rhombus-operator-precedences other-op #:pattern? pattern?))))
  (define (raise-inconsistent how)
    (raise-syntax-error #f
                        (format
                         (string-append "inconsistent operator ~a declared\n"
                                        "  one operator: ~a\n"
                                        "  other operator: ~a")
                         how
                         (syntax-e (rhombus-operator-name op #:pattern? pattern?))
                         (syntax-e (rhombus-operator-name other-op #:pattern? pattern?)))
                        head))
  (when (and dir1 dir2 (not (eq? dir1 dir2)))
    (raise-inconsistent "precedence"))
  (define dir (or dir1 dir2
                  (and (free-identifier=? (rhombus-operator-name op #:pattern? pattern?)
                                          (rhombus-operator-name other-op #:pattern? pattern?))
                       'same)))
  (cond
    [(eq? 'same dir)
     (define op-a (rhombus-infix-operator-assoc op #:pattern? pattern?))
     (when (rhombus-infix-operator? other-op)
       (unless (eq? op-a (rhombus-infix-operator-assoc other-op #:pattern? pattern?))
         (raise-inconsistent "associativity")))
     (case op-a
       [(left) 'weaker]
       [(right) 'stronger]
       [else 'same])]
    [else dir]))

(define (lookup-prefix-implicit alone-name adj-context #:pattern? pattern?)
  (define op-stx (datum->syntax adj-context alone-name))
  (define v (syntax-local-value op-stx (lambda () #f)))
  (unless (if pattern?
              (rhombus-prefix-pattern-operator? v)
              (rhombus-prefix-operator? v))
    (raise-syntax-error #f
                        (format (string-append
                                 "misplaced expression;\n"
                                 " no infix operator is between this expression and the previous one,\n"
                                 " and `~a` is not bound as an implicit prefix ~aoperator")
                                alone-name
                                (if pattern? "pattern " ""))
                        adj-context))
  (values v op-stx))

(define (lookup-infix-implicit adjacent-name adj-context #:pattern? pattern?)
  (define op-stx (datum->syntax adj-context adjacent-name))
  (define v (syntax-local-value op-stx (lambda () #f)))
  (unless (if pattern?
              (rhombus-infix-pattern-operator? v)
              (rhombus-infix-operator? v))
    (raise-syntax-error #f
                        (format
                         (string-append
                          "misplaced expression;\n"
                          " no infix operator is between this expression and the previous one,\n"
                          " and `~a` is not bound as an implicit infix ~aoperator")
                         adjacent-name
                         (if pattern? "pattern " ""))
                        adj-context))
  (values v op-stx))

(define (apply-prefix-operator v form stx #:pattern? pattern?)
  (define proc (rhombus-prefix-operator-proc v #:pattern? pattern?))
  (cond
    [pattern?
     (define-values (ids filter-form) (proc form stx))
     (check-pattern-result ids filter-form proc)]
    [else
     (define new-form (proc form stx))
     (check-expression-result new-form proc)]))

(define (apply-infix-operator v form1 form2 stx #:pattern? pattern?)
  (define proc (rhombus-infix-operator-proc v #:pattern? pattern?))
  (cond
    [pattern?
     (define-values (ids filter-form) (proc form1 form2 stx))
     (check-pattern-result ids filter-form proc)]
    [else
     (define form (proc form1 form2 stx))
     (check-expression-result form proc)]))

(define (apply-prefix-operator-transformer v tail #:pattern? pattern?)
  (define proc (rhombus-prefix-operator-proc v #:pattern? pattern?))
  (cond
    [pattern?
     (define-values (ids filter-form new-tail) (proc tail))
     (check-transformer-result (check-pattern-result ids filter-form proc)
                               new-tail
                               proc)]
    [else
     (define-values (form new-tail) (proc tail))
     (check-transformer-result (check-expression-result form proc)
                               new-tail
                               proc)]))

(define (apply-infix-operator-transformer v form1 tail #:pattern? pattern?)
  (define proc (rhombus-infix-operator-proc v #:pattern? pattern?))
  (cond
    [pattern?
     (define-values (ids filter-form new-tail) (proc form1 tail))
     (check-transformer-result (check-pattern-result ids filter-form proc)
                               new-tail
                               proc)]
    [else
     (define-values (form new-tail) (proc form1 tail))
     (check-transformer-result (check-expression-result form proc)
                               new-tail
                               proc)]))
