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

(provide rhombus-operator-name
         rhombus-operator-less-than-names
         rhombus-operator-same-as-names
         rhombus-operator-greater-than-names

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

         rhombus-prefix-operator-transformer?
         prop:rhombus-prefix-operator-transformer

         rhombus-prefix-pattern-operator-transformer?
         prop:rhombus-prefix-pattern-operator-transformer
         
         rhombus-infix-operator-transformer?
         prop:rhombus-infix-operator-transformer
         
         rhombus-infix-pattern-operator-transformer?
         prop:rhombus-infix-pattern-operator-transformer

         rhombus-multi-prefix-operator
         rhombus-multi-prefix-operator?
         prop:rhombus-multi-prefix-operator
         
         rhombus-multi-prefix-pattern-operator
         rhombus-multi-prefix-pattern-operator?
         prop:rhombus-multi-prefix-pattern-operator

         rhombus-multi-infix-operator
         rhombus-multi-infix-operator?
         prop:rhombus-multi-infix-operator
         
         rhombus-multi-infix-pattern-operator
         rhombus-multi-infix-pattern-operator?
         prop:rhombus-multi-infix-pattern-operator

         relative-precedence

         lookup-implicit-combine

         apply-prefix-operator
         apply-infix-operator
         apply-prefix-operator-transformer
         apply-infix-operator-transformer)

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
                             (list (cons prop:rhombus-infix-operator values))))
(define-values (prop:rhombus-prefix-pattern-operator-transformer rhombus-prefix-pattern-operator-transformer? rhombus-prefix-pattern-operator-transformer-ref)
  (make-struct-type-property 'rhombus-prefix-pattern-operator-transformer #f
                             (list (cons prop:rhombus-infix-pattern-operator values))))
(define-values (prop:rhombus-infix-operator-transformer rhombus-infix-operator-transformer? rhombus-infix-operator-transformer-ref)
  (make-struct-type-property 'rhombus-infix-operator-transformer #f
                             (list (cons prop:rhombus-infix-operator values))))
(define-values (prop:rhombus-infix-pattern-operator-transformer rhombus-infix-pattern-operator-transformer? rhombus-infix-pattern-operator-transformer-ref)
  (make-struct-type-property 'rhombus-infix-pattern-operator-transformer #f
                             (list (cons prop:rhombus-infix-pattern-operator values))))

(struct operator (name less-than-names same-as-names greater-than-names proc))

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
(define (rhombus-prefix-operator name less-than-names same-as-names greater-than-names proc)
  (prefix-operator name less-than-names same-as-names greater-than-names proc))
(define (rhombus-prefix-pattern-operator name less-than-names same-as-names greater-than-names proc)
  (prefix-pattern-operator name less-than-names same-as-names greater-than-names proc))
(define (rhombus-infix-operator name less-than-names same-as-names greater-than-names proc assoc)
  (infix-operator name less-than-names same-as-names greater-than-names proc assoc))
(define (rhombus-infix-pattern-operator name less-than-names same-as-names greater-than-names proc assoc)
  (infix-pattern-operator name less-than-names same-as-names greater-than-names proc assoc))

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

(define (rhombus-operator-less-than-names o #:pattern? pattern?)
  (operator-less-than-names (unwrap-operator o #:pattern? pattern?)))

(define (rhombus-operator-same-as-names o #:pattern? pattern?)
  (operator-same-as-names (unwrap-operator o #:pattern? pattern?)))

(define (rhombus-operator-greater-than-names o #:pattern? pattern?)
  (operator-greater-than-names (unwrap-operator o #:pattern? pattern?)))

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

;; for `#%tuple` and `#%array`:
(define-values (prop:rhombus-multi-prefix-operator rhombus-multi-prefix-operator? rhombus-multi-prefix-operator-ref)
  (make-struct-type-property 'rhombus-multi-prefix-operator))
(define-values (prop:rhombus-multi-prefix-pattern-operator rhombus-multi-prefix-pattern-operator? rhombus-multi-prefix-pattern-operator-ref)
  (make-struct-type-property 'rhombus-multi-prefix-pattern-operator))
(struct multi-prefix-operator (proc)
  #:property prop:rhombus-multi-prefix-operator (lambda (self) self))
(struct multi-prefix-pattern-operator multi-prefix-operator ()
  #:property prop:rhombus-multi-prefix-pattern-operator (lambda (self) self))

;; for `#%call` and `#%ref`:
(define-values (prop:rhombus-multi-infix-operator rhombus-multi-infix-operator? rhombus-multi-infix-operator-ref)
  (make-struct-type-property 'rhombus-multi-infix-operator))
(define-values (prop:rhombus-multi-infix-pattern-operator rhombus-multi-infix-pattern-operator? rhombus-multi-infix-pattern-operator-ref)
  (make-struct-type-property 'rhombus-multi-infix-pattern-operator))
(struct multi-infix-operator (proc)
  #:property prop:rhombus-multi-infix-operator (lambda (self) self))
(struct multi-infix-pattern-operator multi-infix-operator ()
  #:property prop:rhombus-multi-infix-pattern-operator (lambda (self) self))

(define (rhombus-multi-prefix-operator-proc o #:pattern? pattern?)
  (define sel (if pattern?
                  (rhombus-multi-prefix-pattern-operator-ref o #f)
                  (rhombus-multi-prefix-operator-ref o #f)))
  (multi-prefix-operator-proc (sel o)))

(define (rhombus-multi-infix-operator-proc o #:pattern? pattern?)
  (define sel (if pattern?
                  (rhombus-multi-infix-pattern-operator-ref o #f)
                  (rhombus-multi-infix-operator-ref o #f)))
  (multi-infix-operator-proc (sel o)))

;; covenience
(define (rhombus-multi-prefix-operator proc) (multi-prefix-operator proc))
(define (rhombus-multi-prefix-pattern-operator proc) (multi-prefix-pattern-operator proc))
(define (rhombus-multi-infix-operator proc) (multi-infix-operator proc))
(define (rhombus-multi-infix-pattern-operator proc) (multi-infix-pattern-operator proc))  

;; returns: 'higher, 'lower, 'same (no associativity), #f (not related)
(define (relative-precedence op other-op head #:pattern? pattern?)
  (define (find op ids)
    (for/or ([id (in-list ids)])
      (free-identifier=? (rhombus-operator-name op #:pattern? pattern?) id)))
  (define op-lo? (find other-op (rhombus-operator-less-than-names op #:pattern? pattern?)))
  (define op-same? (find other-op (rhombus-operator-same-as-names op #:pattern? pattern?)))
  (define op-hi? (find other-op (rhombus-operator-greater-than-names op #:pattern? pattern?)))
  (define ot-lo? (find op (rhombus-operator-less-than-names other-op #:pattern? pattern?)))
  (define ot-same? (find op (rhombus-operator-same-as-names other-op #:pattern? pattern?)))
  (define ot-hi? (find op (rhombus-operator-greater-than-names other-op #:pattern? pattern?)))
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
  (cond
    [(or (and op-lo? (or ot-lo? ot-same?))
         (and op-same? (or ot-lo? ot-hi?))
         (and op-hi? (or ot-hi? ot-same?)))
     (raise-inconsistent "precedence")]
    [(or op-lo? ot-hi?) 'lower]
    [(or op-hi? ot-lo?) 'higher]
    [(or op-same? ot-same?
         (free-identifier=? (rhombus-operator-name op #:pattern? pattern?)
                            (rhombus-operator-name other-op #:pattern? pattern?)))
     (define op-a (rhombus-infix-operator-assoc op #:pattern? pattern?))
     (when (rhombus-infix-operator? other-op)
       (unless (eq? op-a (rhombus-infix-operator-assoc other-op #:pattern? pattern?))
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
(define (lookup-implicit-combine init-form alone-name adjacent-name context
                                 #:multi? [multi? #f]
                                 #:pattern? pattern?)
  (cond
    [(not init-form)
     (if alone-name
         (lookup-alone-combine context alone-name #:multi? multi? #:pattern? pattern?)
         (lambda (init-form form) form))]
    [else
     (lookup-adjacent-combine context adjacent-name #:multi? multi? #:pattern? pattern?)]))

(define (lookup-alone-combine adj-context alone-name #:multi? multi? #:pattern? pattern?)
  (define v (syntax-local-value (datum->syntax adj-context alone-name) (lambda () #f)))
  (unless (if multi?
              (if pattern?
                  (rhombus-multi-prefix-pattern-operator? v)
                  (rhombus-multi-prefix-operator? v))
              (if pattern?
                  (rhombus-prefix-pattern-operator? v)
                  (rhombus-prefix-operator? v)))
    (raise-syntax-error #f
                        (format (string-append
                                 "misplaced expression;\n"
                                 " no infix operator is between this expression and the previous one,\n"
                                 " and `~a` is not bound as an implicit ~aprefix ~aoperator")
                                alone-name
                                (if multi? "multi-" "")
                                (if pattern? "pattern " ""))
                        adj-context))
  (lambda (init-form form/s)
    (apply-prefix-operator v form/s adj-context #:multi? multi? #:pattern? pattern?)))

(define (lookup-adjacent-combine adj-context adjacent-name #:multi? multi? #:pattern? pattern?)
  (define v (syntax-local-value (datum->syntax adj-context adjacent-name) (lambda () #f)))
  (unless (if multi?
              (if pattern?
                  (rhombus-multi-infix-pattern-operator? v)
                  (rhombus-multi-infix-operator? v))
              (if pattern?
                  (rhombus-infix-pattern-operator? v)
                  (rhombus-infix-operator? v)))
    (raise-syntax-error #f
                        (format
                         (string-append
                          "misplaced expression;\n"
                          " no infix operator is between this expression and the previous one,\n"
                          " and `~a` is not bound as an implicit ~ainfix ~aoperator")
                         adjacent-name
                         (if multi? "multi-" "")
                         (if pattern? "pattern " ""))
                        adj-context))
  (lambda (form1 form2/s)
    (apply-infix-operator v form1 form2/s adj-context #:multi? multi? #:pattern? pattern?)))

(define (apply-prefix-operator v form/s stx #:multi? [multi? #f] #:pattern? pattern?)
  (define proc (if multi?
                   (rhombus-multi-prefix-operator-proc v #:pattern? pattern?)
                   (rhombus-prefix-operator-proc v #:pattern? pattern?)))
  (cond
    [pattern?
     (define-values (ids filter-form) (proc form/s stx))
     (check-pattern-result ids filter-form proc)]
    [else
     (define form (proc form/s stx))
     (check-expression-result form proc)]))

(define (apply-infix-operator v form1 form2/s stx #:multi? [multi? #f] #:pattern? pattern?)
  (define proc (if multi?
                   (rhombus-multi-infix-operator-proc v #:pattern? pattern?)
                   (rhombus-infix-operator-proc v #:pattern? pattern?)))
  (cond
    [pattern?
     (define-values (ids filter-form) (proc form1 form2/s stx))
     (check-pattern-result ids filter-form proc)]
    [else
     (define form (proc form1 form2/s stx))
     (check-expression-result form proc)]))

(define (apply-prefix-operator-transformer v tail stx #:pattern? pattern?)
  (define proc (rhombus-prefix-operator-proc v #:pattern? pattern?))
  (cond
    [pattern?
     (define-values (ids filter-form new-tail) (proc tail stx))
     (check-transformer-result (check-pattern-result ids filter-form proc)
                               new-tail
                               proc)]
    [else
     (define-values (form new-tail) (proc tail stx))
     (check-transformer-result (check-expression-result form proc)
                               new-tail
                               proc)]))

(define (apply-infix-operator-transformer v form1 tail stx #:pattern? pattern?)
  (define proc (rhombus-infix-operator-proc v #:pattern? pattern?))
  (cond
    [pattern?
     (define-values (ids filter-form new-tail) (proc form1 tail stx))
     (check-transformer-result (check-pattern-result ids filter-form proc)
                               new-tail
                               proc)]
    [else
     (define-values (form new-tail) (proc form1 tail stx))
     (check-transformer-result (check-expression-result form proc)
                               new-tail
                               proc)]))
