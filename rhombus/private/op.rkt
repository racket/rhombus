#lang racket/base

(provide rhombus-operator?
         rhombus-operator-name
         rhombus-operator-less-than-names
         rhombus-operator-same-as-names
         rhombus-operator-greater-than-names

         rhombus-unary-operator
         rhombus-unary-operator?
         rhombus-unary-operator-proc
         prop:rhombus-unary-operator

         rhombus-binary-operator
         rhombus-binary-operator?
         rhombus-binary-operator-proc
         rhombus-binary-operator-assoc
         prop:rhombus-binary-operator

         (struct-out rhombus-multi-unary-operator)
         (struct-out rhombus-multi-binary-operator)

         juxtipose-name
         tuple-name
         call-name
         array-name
         ref-name

         relative-precedence)

(define-values (prop:rhombus-unary-operator rhombus-unary-operator? rhombus-unary-operator-ref)
  (make-struct-type-property 'rhombus-unary-operator))
(define-values (prop:rhombus-binary-operator rhombus-binary-operator? rhombus-binary-operator-ref)
  (make-struct-type-property 'rhombus-binary-operator))

(struct operator (name less-than-names same-as-names greater-than-names))
(struct unary-operator operator (proc)
  #:property prop:rhombus-unary-operator (lambda (self) self))
(struct binary-operator operator (assoc proc)
  #:property prop:rhombus-binary-operator (lambda (self) self))

(define (rhombus-unary-operator name less-than-names same-as-names greater-than-names proc)
  (unary-operator name less-than-names same-as-names greater-than-names proc))

(define (rhombus-binary-operator name less-than-names same-as-names greater-than-names assoc proc)
  (binary-operator name less-than-names same-as-names greater-than-names assoc proc))

(define (rhombus-operator? v)
  (or (rhombus-unary-operator? v)
      (rhombus-binary-operator? v)))

(define (unwrap-operator o)
  (cond
    [(or (rhombus-unary-operator-ref o #f)
         (rhombus-binary-operator-ref o #f))
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

(define (unwrap-unary-operator o)
  (cond
    [(rhombus-unary-operator-ref o #f)
     => (lambda (sel) (sel o))]
    [else o]))

(define (rhombus-unary-operator-proc o)
  (unary-operator-proc (unwrap-unary-operator o)))

(define (unwrap-binary-operator o)
  (cond
    [(rhombus-binary-operator-ref o #f)
     => (lambda (sel) (sel o))]
    [else o]))

(define (rhombus-binary-operator-proc o)
  (binary-operator-proc (unwrap-binary-operator o)))

(define (rhombus-binary-operator-assoc o)
  (binary-operator-assoc (unwrap-binary-operator o)))

;; for `#%tuple` and `#%array`:
(struct rhombus-multi-unary-operator (proc))
;; for `#%call` and `#%ref`:
(struct rhombus-multi-binary-operator (proc))

(define juxtipose-name '#%juxtipose)
(define tuple-name '#%tuple)
(define call-name '#%call)
(define array-name '#%array)
(define ref-name '#%ref)

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
     (define op-a (rhombus-binary-operator-assoc op))
     (when (rhombus-binary-operator? other-op)
       (unless (eq? op-a (rhombus-binary-operator-assoc other-op))
         (raise-inconsistent "associativity")))
     (case op-a
       [(left) 'lower]
       [(right) 'higher]
       [else 'same])]
    [else #f]))
