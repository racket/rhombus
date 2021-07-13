#lang racket/base
(require syntax/stx
         "proc-name.rkt")

;; See "parse.rkt" for general information about operators and parsing.
;;
;; The compile-time nature of being a unary operator, binary
;; operator, etc., is represented using structure-type properties, so
;; a name can be bound to multiple of those (e.g., `-` is normally
;; both a unary and a binary operator). An binding cannot be both a
;; transformer binary operator and non-transformer binary operator,
;; however, because the transformer nature takes precedence.

(provide rhombus-operator?
         rhombus-operator-name
         rhombus-operator-less-than-names
         rhombus-operator-same-as-names
         rhombus-operator-greater-than-names

         rhombus-prefix-operator
         rhombus-prefix-operator?
         rhombus-prefix-operator-proc
         prop:rhombus-prefix-operator

         rhombus-infix-operator
         rhombus-infix-operator?
         rhombus-infix-operator-proc
         rhombus-infix-operator-assoc
         prop:rhombus-infix-operator

         rhombus-prefix-operator-transformer?
         prop:rhombus-prefix-operator-transformer
         rhombus-infix-operator-transformer?
         prop:rhombus-infix-operator-transformer
         
         (struct-out rhombus-multi-prefix-operator)
         (struct-out rhombus-multi-infix-operator)

         relative-precedence

         lookup-implicit-combine

         apply-prefix-operator
         apply-infix-operator
         apply-prefix-operator-transformer
         apply-infix-operator-transformer)

(define-values (prop:rhombus-prefix-operator rhombus-prefix-operator? rhombus-prefix-operator-ref)
  (make-struct-type-property 'rhombus-prefix-operator))
(define-values (prop:rhombus-infix-operator rhombus-infix-operator? rhombus-infix-operator-ref)
  (make-struct-type-property 'rhombus-infix-operator))

(define-values (prop:rhombus-prefix-operator-transformer rhombus-prefix-operator-transformer? rhombus-prefix-operator-transformer-ref)
  (make-struct-type-property 'rhombus-prefix-operator-transformer #f
                             (list (cons prop:rhombus-infix-operator values))))
(define-values (prop:rhombus-infix-operator-transformer rhombus-infix-operator-transformer? rhombus-infix-operator-transformer-ref)
  (make-struct-type-property 'rhombus-infix-operator-transformer #f
                             (list (cons prop:rhombus-infix-operator values))))

(struct operator (name less-than-names same-as-names greater-than-names))
(struct prefix-operator operator (proc)
  #:property prop:rhombus-prefix-operator (lambda (self) self))
(struct infix-operator operator (assoc proc)
  #:property prop:rhombus-infix-operator (lambda (self) self))

(define (rhombus-prefix-operator name less-than-names same-as-names greater-than-names proc)
  (prefix-operator name less-than-names same-as-names greater-than-names proc))

(define (rhombus-infix-operator name less-than-names same-as-names greater-than-names assoc proc)
  (infix-operator name less-than-names same-as-names greater-than-names assoc proc))

(define (rhombus-operator? v)
  (or (rhombus-prefix-operator? v)
      (rhombus-infix-operator? v)))

(define (unwrap-operator o)
  (cond
    [(or (rhombus-prefix-operator-ref o #f)
         (rhombus-infix-operator-ref o #f))
     => (lambda (sel) (sel o))]
    [else o]))

(define (rhombus-operator-name o)
  (operator-name (unwrap-operator o)))

(define (rhombus-operator-less-than-names o)
  (operator-less-than-names (unwrap-operator o)))

(define (rhombus-operator-same-as-names o)
  (operator-same-as-names (unwrap-operator o)))

(define (rhombus-operator-greater-than-names o)
  (operator-greater-than-names (unwrap-operator o)))

(define (unwrap-prefix-operator o)
  (cond
    [(rhombus-prefix-operator-ref o #f)
     => (lambda (sel) (sel o))]
    [else o]))

(define (rhombus-prefix-operator-proc o)
  (prefix-operator-proc (unwrap-prefix-operator o)))

(define (unwrap-infix-operator o)
  (cond
    [(rhombus-infix-operator-ref o #f)
     => (lambda (sel) (sel o))]
    [else o]))

(define (rhombus-infix-operator-proc o)
  (infix-operator-proc (unwrap-infix-operator o)))

(define (rhombus-infix-operator-assoc o)
  (infix-operator-assoc (unwrap-infix-operator o)))

;; for `#%tuple` and `#%array`:
(struct rhombus-multi-prefix-operator (proc))
;; for `#%call` and `#%ref`:
(struct rhombus-multi-infix-operator (proc))

;; returns: 'higher, 'lower, 'same (no associativity), #f (not related)
(define (relative-precedence op other-op head)
  (define (find op ids)
    (for/or ([id (in-list ids)])
      (free-identifier=? (rhombus-operator-name op) id)))
  (define op-lo? (find other-op (rhombus-operator-less-than-names op)))
  (define op-same? (find other-op (rhombus-operator-same-as-names op)))
  (define op-hi? (find other-op (rhombus-operator-greater-than-names op)))
  (define ot-lo? (find op (rhombus-operator-less-than-names other-op)))
  (define ot-same? (find op (rhombus-operator-same-as-names other-op)))
  (define ot-hi? (find op (rhombus-operator-greater-than-names other-op)))
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
  (cond
    [(or (and op-lo? (or ot-lo? ot-same?))
         (and op-same? (or ot-lo? ot-hi?))
         (and op-hi? (or ot-hi? ot-same?)))
     (raise-inconsistent "precedence")]
    [(or op-lo? ot-hi?) 'lower]
    [(or op-hi? ot-lo?) 'higher]
    [(or op-same? ot-same?
         (free-identifier=? (rhombus-operator-name op)
                            (rhombus-operator-name other-op)))
     (define op-a (rhombus-infix-operator-assoc op))
     (when (rhombus-infix-operator? other-op)
       (unless (eq? op-a (rhombus-infix-operator-assoc other-op))
         (raise-inconsistent "associativity")))
     (case op-a
       [(left) 'lower]
       [(right) 'higher]
       [else 'same])]
    [else #f]))

;; Helper to find an implicit, which is constrained to have a higher
;; precedence than everything else and to be left-associative.
;; For example, the function-call form triggered by `f(x)` is an
;; implement that combines `f` and `(x)`; implicits can have multiple
;; RHS arguments, as in `f(x, y)` or `a[row, col]`. Implicits are also
;; used for parenthesized things or brackets things without a preceding
;; expression. Finally, there can be an implicit that combines arbitrary
;; adjacent expressions where the second is not in parentheses or braces
(define (lookup-implicit-combine init-form alone-name adjacent-name context #:multi? [multi? #f])
  (cond
    [(not init-form)
     (if alone-name
         (lookup-alone-combine context alone-name #:multi? multi?)
         (lambda (init-form form) form))]
    [else
     (lookup-adjacent-combine context adjacent-name #:multi? multi?)]))

(define (lookup-alone-combine adj-context alone-name #:multi? multi?)
  (define v (syntax-local-value (datum->syntax adj-context alone-name) (lambda () #f)))
  (unless (if multi?
              (rhombus-multi-prefix-operator? v)
              (rhombus-prefix-operator? v))
    (raise-syntax-error #f
                        (format (string-append
                                 "misplaced expression;\n"
                                 " no infix operator is between this expression and the previous one,\n"
                                 " and `~a` is not bound as an implicit ~aprefix operator")
                                alone-name
                                (if multi? "multi-" ""))
                        adj-context))
  (lambda (init-form form/s)
    (apply-prefix-operator v form/s adj-context #:multi? multi?)))

(define (lookup-adjacent-combine adj-context adjacent-name #:multi? multi?)
  (define v (syntax-local-value (datum->syntax adj-context adjacent-name) (lambda () #f)))
  (unless (if multi?
              (rhombus-multi-infix-operator? v)
              (rhombus-infix-operator? v))
    (raise-syntax-error #f
                        (format
                         (string-append
                          "misplaced expression;\n"
                          " no infix operator is between this expression and the previous one,\n"
                          " and `~a` is not bound as an implicit ~ainfix operator")
                         adjacent-name
                         (if multi? "multi-" ""))
                        adj-context))
  (lambda (form1 form2/s)
    (apply-infix-operator v form1 form2/s adj-context #:multi? multi?)))

(define (apply-prefix-operator v form/s stx #:multi? [multi? #f])
  (define proc (if multi?
                   (rhombus-multi-prefix-operator-proc v)
                   (rhombus-prefix-operator-proc v)))
  (define form (proc form/s stx))
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  form)

(define (apply-infix-operator v form1 form2/s stx #:multi? [multi? #f])
  (define proc (if multi?
                    (rhombus-multi-infix-operator-proc v)
                    (rhombus-infix-operator-proc v)))
  (define form (proc form1 form2/s stx))
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  form)

(define (apply-prefix-operator-transformer v tail stx)
  (define proc (rhombus-prefix-operator-proc v))
  (define-values (form new-tail) (proc tail stx))
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  (unless (stx-list? form) (raise-result-error (proc-name proc) "stx-list?" form))
  (values form new-tail))

(define (apply-infix-operator-transformer v form1 tail stx)
  (define proc (rhombus-infix-operator-proc v))
  (define-values (form new-tail) (proc form1 tail stx))
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  (unless (stx-list? form) (raise-result-error (proc-name proc) "stx-list?" form))
  (values form new-tail))
