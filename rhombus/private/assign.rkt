#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     enforest/syntax-local
                     enforest/hier-name-parse
                     enforest/operator
                     "name-path-op.rkt"
                     "annotation-string.rkt"
                     "srcloc.rkt")
         "binding.rkt"
         "expression.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
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
             #:with tail #'name.tail))

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
       [(_ id:identifier c::inline-annotation)
        (values
         (binding-form
          #'mutable-info
          #'[id c.converter c.static-infos c.annotation-str])
         #'())]
       [(_ id:identifier . new-tail)
        (values
         (binding-form
          #'mutable-info
          #'[id #f () #f])
         #'new-tail)]))))

(define-syntax (mutable-info stx)
  (syntax-parse stx
    [(_ right-static-infos [id converter static-infos annotation-str])
     (binding-info annotation-any-string
                   #'id
                   #'() ; forget enclosing  static-infos, since we can't enforce them on mutation
                   #'((id (0)))
                   #'mutable-identifier-succeed
                   #'mutable-commit
                   #'mutable-bind
                   #'[id mutable-id converter-id converter static-infos annotation-str])]))

(define-syntax (mutable-identifier-succeed stx)
  (syntax-parse stx
    [(_ arg-id [bind-id mutable-id converter-id convert static-infos annotation-str] IF success fail)
     #`(begin
         #,@(if (syntax-e #'convert)
                #`((define converter-id (lambda (val)
                                          (convert val
                                                   '(bind-id . annotation-str)
                                                   raise-mutable-binding-annotation-fail)))
                   (define mutable-id (converter-id arg-id)))
                #`((define mutable-id arg-id)))
         (set! mutable-id mutable-id)
         (IF #t success fail))]))

(define-syntax (mutable-commit stx)
  (syntax-parse stx
    [(_ arg-id _)
     #'(begin)]))

(define-syntax (mutable-bind stx)
  (syntax-parse stx
    [(_ arg-id [bind-id mutable-id converter-id convert static-infos annotation-str])
     #`(define-syntax bind-id
         (make-mutable-variable #'mutable-id
                                #,(and (syntax-e #'convert)
                                       #'(quote-syntax converter-id))
                                (quote-syntax static-infos)))]))

(begin-for-syntax
  (struct mutable-variable expression-prefix-operator (id convert-id static-infos)
    #:property prop:rename-transformer (struct-field-index id))
  (define (make-mutable-variable id converter-id static-infos)
    (mutable-variable (quote-syntax unused)
                      '((default . stronger))
                      'macro
                      (lambda (stx)
                        (syntax-parse stx
                          [(self . tail)
                           (values (wrap-static-info* #'self static-infos)
                                   #'tail)]))
                      (syntax-property id 'not-free-identifier=? #t)
                      converter-id
                      static-infos))
  (define (mutable-variable-ref v) (and (mutable-variable? v) v)))

(define (raise-mutable-binding-annotation-fail val who)
  (raise-annotation-failure (car who) val (cdr who)))

(define-for-syntax (make-assign-infix-operator name prec assc protocol proc)
  (define (get-mv form1 self-stx)
    (define inside (syntax-parse (unwrap-static-infos form1)
                     #:literals (rhombus-expression)
                     #:datum-literals (group)
                     [(rhombus-expression (group iform)) #'iform]
                     [iform #'iform]))
    (define mv (and (identifier? inside)
                    (syntax-local-value* inside mutable-variable-ref)))
    (unless mv
      (raise-syntax-error #f
                          "left-hand argument is not a mutable identifier"
                          self-stx))
    (values mv inside))
  (define (convert mv id)
    (define convert-id (mutable-variable-convert-id mv))
    (if convert-id
        #`(#,convert-id #,id)
        id))
  (assign-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 self-stx)
         (define-values (mv inside) (get-mv form1 self-stx))
         (relocate
          (respan (datum->syntax #f (list form1 self-stx form2)))
          #`(let ([#,inside #,form2]) ; using `inside` here provides a name to `form2`
              #,(build-assign/automatic proc
                                        self-stx
                                        #`(lambda ()
                                            #,(mutable-variable-id mv))
                                        #`(lambda (v)
                                            (set! #,(mutable-variable-id mv) #,(convert mv #'v)))
                                        inside
                                        inside))))
       (lambda (form1 tail)
         (syntax-parse tail
           [(head . tail)
            (define self-stx #'head)
            (define-values (mv inside) (get-mv form1 self-stx))
            (build-assign/macro proc
                                self-stx
                                #`(lambda ()
                                    #,(mutable-variable-id mv))
                                #`(lambda (v)
                                    (set! #,(mutable-variable-id mv) #,(convert mv #'v)))
                                inside
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
