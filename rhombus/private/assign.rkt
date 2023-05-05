#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     enforest/syntax-local
                     enforest/hier-name-parse
                     enforest/operator
                     "name-path-op.rkt"
                     "annotation-string.rkt")
         "binding.rkt"
         "expression.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "parse.rkt")

(provide :=
         (for-space rhombus/bind
                    mutable))

(module+ for-assign
  (provide (for-syntax make-assign-infix-operator
                       build-assign
                       assign-infix-operator-ref
                       :assign-op-seq)))

(begin-for-syntax
  (property assign-infix-operator expression-infix-operator (assign-proc))

  (define-syntax-class :assign-op-seq
    #:description "assignment operation"
    #:attributes (op name tail)
    (pattern (~var name (:hier-name-seq in-name-root-space in-expression-space name-path-op name-root-ref))
             #:do [(define op (syntax-local-value* #'name.name assign-infix-operator-ref))]
             #:when op
             #:attr op op
             #:attr tail #'name.tail))

  (define (build-assign/automatic proc self-stx ref set rhs-name rhs)
    (proc ref set rhs self-stx rhs-name))

  (define (build-assign/macro proc self-stx ref set rhs-name tail)
    (proc ref set (cons self-stx tail) rhs-name))

  (define (build-assign op self-stx ref set rhs-name tail)
    (cond
      [(eq? (operator-protocol op) 'automatic)
       (syntax-parse #`(group . #,tail)
         [(~var e (:infix-op+expression+tail (operator-name op)))
          (values (build-assign/automatic (assign-infix-operator-assign-proc op) self-stx ref set rhs-name
                                          #`(let ([#,rhs-name e.parsed])
                                              #,rhs-name))
                  #'e.tail)])]
      [else
       (build-assign/macro (assign-infix-operator-assign-proc op) self-stx ref set rhs-name tail)])))

(define-binding-syntax mutable
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier . new-tail)
        (values
         (binding-form
          #'mutable-info
          #'id)
         #'new-tail)]))))

(define-syntax (mutable-info stx)
  (syntax-parse stx
    [(_ static-infos id)
     (binding-info annotation-any-string
                   #'id
                   #'() ; mutable => don't claim input's static info
                   #'((id (0)))
                   #'mutable-identifier-succeed
                   #'mutable-commit
                   #'mutable-bind
                   #'[id mutable-id])]))

(define-syntax (mutable-identifier-succeed stx)
  (syntax-parse stx
    [(_ arg-id [bind-id mutable-id] IF success fail)
     #'(begin
         (define mutable-id arg-id)
         (set! mutable-id mutable-id)
         (IF #t success fail))]))

(define-syntax (mutable-commit stx)
  (syntax-parse stx
    [(_ arg-id [bind-id mutable-id])
     #'(begin)]))

(define-syntax (mutable-bind stx)
  (syntax-parse stx
    [(_ arg-id [bind-id mutable-id])
     #'(define-syntax bind-id
         (mutable-variable #'mutable-id))]))

(begin-for-syntax
  (struct mutable-variable (id)
    #:property prop:rename-transformer (struct-field-index id))
  (define (mutable-variable-ref v) (and (mutable-variable? v) v)))

(define-for-syntax (make-assign-infix-operator name prec assc protocol proc)
  (define (get-mv form1 self-stx)
    (define mv (and (identifier? form1)
                    (syntax-local-value* form1 mutable-variable-ref)))
    (unless mv
      (raise-syntax-error #f
                          "left-hand argument is not a mutable identifier"
                          self-stx))
    mv)
  (assign-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 self-stx)
         (define mv (get-mv form1 self-stx))
         #`(let ([#,form1 #,form2]) ; using `form1` here provides a name to `form2`
             #,(build-assign/automatic proc
                                       self-stx
                                       #`(lambda ()
                                           #,(mutable-variable-id mv))
                                       #`(lambda (v)
                                           (set! #,(mutable-variable-id mv) v))
                                       form1
                                       form1)))
       (lambda (form1 tail)
         (syntax-parse tail
           [(head . tail)
            (define self-stx #'head)
            (define mv (get-mv form1 self-stx))
            (build-assign/macro proc
                                self-stx
                                #`(lambda ()
                                    #,(mutable-variable-id mv))
                                #`(lambda (v)
                                    (set! #,(mutable-variable-id mv) v))
                                form1
                                #'tail)])))
   assc
   proc))

(define-syntax :=
  (make-assign-infix-operator
   (expr-quote :=)
   '((default . weaker))
   'left
   'automatic
   (lambda (left-ref-stx left-assign-stx right-stx self-stx rhs-name)
     #`(#,left-assign-stx #,right-stx))))
