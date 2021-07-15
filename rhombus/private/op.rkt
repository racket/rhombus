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

         rhombus-prefix-binding-operator
         rhombus-prefix-binding-operator?
         prop:rhombus-prefix-binding-operator

         rhombus-infix-operator-assoc
         rhombus-infix-operator-proc
         
         rhombus-infix-operator
         rhombus-infix-operator?
         prop:rhombus-infix-operator

         rhombus-infix-binding-operator
         rhombus-infix-binding-operator?
         prop:rhombus-infix-binding-operator

         rhombus-prefix-operator-transformer
         rhombus-prefix-operator-transformer?
         prop:rhombus-prefix-operator-transformer

         rhombus-prefix-binding-operator-transformer
         rhombus-prefix-binding-operator-transformer?
         prop:rhombus-prefix-binding-operator-transformer

         rhombus-infix-operator-transformer
         rhombus-infix-operator-transformer?
         prop:rhombus-infix-operator-transformer

         rhombus-infix-binding-operator-transformer
         rhombus-infix-binding-operator-transformer?
         prop:rhombus-infix-binding-operator-transformer)

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
(define-values (prop:rhombus-prefix-binding-operator rhombus-prefix-binding-operator? rhombus-prefix-binding-operator-ref)
  (make-struct-type-property 'rhombus-prefix-binding-operator))
(define-values (prop:rhombus-infix-operator rhombus-infix-operator? rhombus-infix-operator-ref)
  (make-struct-type-property 'rhombus-infix-operator))
(define-values (prop:rhombus-infix-binding-operator rhombus-infix-binding-operator? rhombus-infix-binding-operator-ref)
  (make-struct-type-property 'rhombus-infix-binding-operator))

(define-values (prop:rhombus-prefix-operator-transformer rhombus-prefix-operator-transformer? rhombus-prefix-operator-transformer-ref)
  (make-struct-type-property 'rhombus-prefix-operator-transformer #f
                             (list (cons prop:rhombus-prefix-operator values))))
(define-values (prop:rhombus-prefix-binding-operator-transformer rhombus-prefix-binding-operator-transformer? rhombus-prefix-binding-operator-transformer-ref)
  (make-struct-type-property 'rhombus-prefix-binding-operator-transformer #f
                             (list (cons prop:rhombus-prefix-binding-operator values))))
(define-values (prop:rhombus-infix-operator-transformer rhombus-infix-operator-transformer? rhombus-infix-operator-transformer-ref)
  (make-struct-type-property 'rhombus-infix-operator-transformer #f
                             (list (cons prop:rhombus-infix-operator values))))
(define-values (prop:rhombus-infix-binding-operator-transformer rhombus-infix-binding-operator-transformer? rhombus-infix-binding-operator-transformer-ref)
  (make-struct-type-property 'rhombus-infix-binding-operator-transformer #f
                             (list (cons prop:rhombus-infix-binding-operator values))))

(struct operator (name precedences proc))

(struct prefix-operator operator ()
  #:property prop:rhombus-prefix-operator (lambda (self) self))
(struct prefix-binding-operator operator ()
  #:property prop:rhombus-prefix-binding-operator (lambda (self) self))

(struct infix-*operator operator (assoc))
(struct infix-operator infix-*operator ()
  #:property prop:rhombus-infix-operator (lambda (self) self))
(struct infix-binding-operator infix-*operator ()
  #:property prop:rhombus-infix-binding-operator (lambda (self) self))

;; convenience functions
(define (rhombus-prefix-operator name precedences proc)
  (prefix-operator name precedences proc))
(define (rhombus-prefix-binding-operator name precedences proc)
  (prefix-binding-operator name precedences proc))
(define (rhombus-infix-operator name precedences assoc proc)
  (infix-operator name precedences proc assoc))
(define (rhombus-infix-binding-operator name precedences assoc proc)
  (infix-binding-operator name precedences proc assoc))

(struct prefix-operator-transformer operator ()
  #:property prop:rhombus-prefix-operator-transformer (lambda (self) self))
(struct prefix-binding-operator-transformer operator ()
  #:property prop:rhombus-prefix-binding-operator-transformer (lambda (self) self))
(struct infix-operator-transformer infix-*operator ()
  #:property prop:rhombus-infix-operator-transformer (lambda (self) self))
(struct infix-binding-operator-transformer infix-*operator ()
  #:property prop:rhombus-infix-binding-operator-transformer (lambda (self) self))

(define (rhombus-prefix-operator-transformer name precedences proc)
  (prefix-operator-transformer name precedences proc))
(define (rhombus-prefix-binding-operator-transformer name precedences proc)
  (prefix-binding-operator-transformer name precedences proc))
(define (rhombus-infix-operator-transformer name precedences assoc proc)
  (infix-operator-transformer name precedences proc assoc))
(define (rhombus-infix-binding-operator-transformer name precedences assoc proc)
  (infix-binding-operator-transformer name precedences proc assoc))

(define (unwrap-operator o #:binding? binding?)
  (cond
    [(if binding?
         (or (rhombus-prefix-binding-operator-ref o #f)
             (rhombus-infix-binding-operator-ref o #f))
         (or (rhombus-prefix-operator-ref o #f)
             (rhombus-infix-operator-ref o #f)))
     => (lambda (sel) (sel o))]
    [else (raise-argument-error 'operator-property (if binding? "binding-operator?" "operator?") o)]))

(define (rhombus-operator-name o #:binding? binding?)
  (operator-name (unwrap-operator o #:binding? binding?)))

(define (rhombus-operator-precedences o #:binding? binding?)
  (operator-precedences (unwrap-operator o #:binding? binding?)))

(define (unwrap-prefix-operator o #:binding? binding?)
  (cond
    [(if binding?
         (rhombus-prefix-binding-operator-ref o #f)
         (rhombus-prefix-operator-ref o #f))
     => (lambda (sel) (sel o))]
    [else (raise-argument-error 'operator-property (if binding? "prefix-binding-operator?" "prefix-operator?") o)]))

(define (rhombus-prefix-operator-proc o #:binding? binding?)
  (operator-proc (unwrap-prefix-operator o #:binding? binding?)))

(define (unwrap-infix-operator o #:binding? binding?)
  (cond
    [(if binding?
         (rhombus-infix-binding-operator-ref o #f)
         (rhombus-infix-operator-ref o #f))
     => (lambda (sel) (sel o))]
    [else (raise-argument-error 'operator-property (if binding? "infix-binding-operator?" "infix-operator?") o)]))

(define (rhombus-infix-operator-proc o #:binding? binding?)
  (operator-proc (unwrap-infix-operator o #:binding? binding?)))

(define (rhombus-infix-operator-assoc o #:binding? binding?)
  (infix-*operator-assoc (unwrap-infix-operator o #:binding? binding?)))

;; returns: 'stronger, 'weaker, 'same (no associativity), #f (not related)
(define (relative-precedence op other-op head #:binding? binding?)
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
  (define op-name (rhombus-operator-name op #:binding? binding?))
  (define other-op-name (rhombus-operator-name other-op #:binding? binding?))
  (define dir1 (find other-op-name op-name (rhombus-operator-precedences op #:binding? binding?)))
  (define dir2 (invert (find op-name other-op-name (rhombus-operator-precedences other-op #:binding? binding?))))
  (define (raise-inconsistent how)
    (raise-syntax-error #f
                        (format
                         (string-append "inconsistent operator ~a declared\n"
                                        "  one operator: ~a\n"
                                        "  other operator: ~a")
                         how
                         (syntax-e (rhombus-operator-name op #:binding? binding?))
                         (syntax-e (rhombus-operator-name other-op #:binding? binding?)))
                        head))
  (when (and dir1 dir2 (not (eq? dir1 dir2)))
    (raise-inconsistent "precedence"))
  (define dir (or dir1 dir2
                  (and (free-identifier=? (rhombus-operator-name op #:binding? binding?)
                                          (rhombus-operator-name other-op #:binding? binding?))
                       'same)))
  (cond
    [(eq? 'same dir)
     (define op-a (rhombus-infix-operator-assoc op #:binding? binding?))
     (when (rhombus-infix-operator? other-op)
       (unless (eq? op-a (rhombus-infix-operator-assoc other-op #:binding? binding?))
         (raise-inconsistent "associativity")))
     (case op-a
       [(left) 'weaker]
       [(right) 'stronger]
       [else 'same])]
    [else dir]))

(define (lookup-prefix-implicit alone-name adj-context #:binding? binding?)
  (define op-stx (datum->syntax adj-context alone-name))
  (define v (syntax-local-value op-stx (lambda () #f)))
  (unless (if binding?
              (rhombus-prefix-binding-operator? v)
              (rhombus-prefix-operator? v))
    (raise-syntax-error #f
                        (format (string-append
                                 "misplaced expression;\n"
                                 " no infix operator is between this expression and the previous one,\n"
                                 " and `~a` is not bound as an implicit prefix ~aoperator")
                                alone-name
                                (if binding? "binding " ""))
                        adj-context))
  (values v op-stx))

(define (lookup-infix-implicit adjacent-name adj-context #:binding? binding?)
  (define op-stx (datum->syntax adj-context adjacent-name))
  (define v (syntax-local-value op-stx (lambda () #f)))
  (unless (if binding?
              (rhombus-infix-binding-operator? v)
              (rhombus-infix-operator? v))
    (raise-syntax-error #f
                        (format
                         (string-append
                          "misplaced expression;\n"
                          " no infix operator is between this expression and the previous one,\n"
                          " and `~a` is not bound as an implicit infix ~aoperator")
                         adjacent-name
                         (if binding? "binding " ""))
                        adj-context))
  (values v op-stx))

(define (apply-prefix-operator v form stx #:binding? binding?)
  (define proc (rhombus-prefix-operator-proc v #:binding? binding?))
  (cond
    [binding?
     (define-values (var-ids matcher-form stx-ids stx-form) (proc form stx))
     (check-binding-result var-ids matcher-form stx-ids stx-form proc)]
    [else
     (define new-form (proc form stx))
     (check-expression-result new-form proc)]))

(define (apply-infix-operator v form1 form2 stx #:binding? binding?)
  (define proc (rhombus-infix-operator-proc v #:binding? binding?))
  (cond
    [binding?
     (define-values (var-ids matcher-form stx-ids stx-form) (proc form1 form2 stx))
     (check-binding-result var-ids matcher-form stx-ids stx-form proc)]
    [else
     (define form (proc form1 form2 stx))
     (check-expression-result form proc)]))

(define (apply-prefix-operator-transformer v tail #:binding? binding?)
  (define proc (rhombus-prefix-operator-proc v #:binding? binding?))
  (cond
    [binding?
     (define-values (var-ids matcher-form stx-ids stx-form new-tail) (proc tail))
     (check-transformer-result (check-binding-result var-ids matcher-form stx-ids stx-form proc)
                               new-tail
                               proc)]
    [else
     (define-values (form new-tail) (proc tail))
     (check-transformer-result (check-expression-result form proc)
                               new-tail
                               proc)]))

(define (apply-infix-operator-transformer v form1 tail #:binding? binding?)
  (define proc (rhombus-infix-operator-proc v #:binding? binding?))
  (cond
    [binding?
     (define-values (var-ids matcher-form stx-ids stx-form new-tail) (proc form1 tail))
     (check-transformer-result (check-binding-result var-ids matcher-form stx-ids stx-form proc)
                               new-tail
                               proc)]
    [else
     (define-values (form new-tail) (proc form1 tail))
     (check-transformer-result (check-expression-result form proc)
                               new-tail
                               proc)]))
