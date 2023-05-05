#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     enforest/transformer-result
                     "pack.rkt"
                     "tail-returner.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
                     (for-syntax racket/base))
         "name-root.rkt"
         "macro-macro.rkt"
         "parse.rkt"
         (submod "assign.rkt" for-assign)
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-space rhombus/namespace
                    assign)
         (for-syntax
          (for-space rhombus/namespace
                     assign_meta)))

(define-name-root assign
  #:fields
  (macro))

(begin-for-syntax
  (define-name-root assign_meta
    #:fields
    (unpack_left
     pack_assignment
     AssignParsed)))

(define-operator-definition-transformer macro
  'macro
  #f
  #'#f
  #'make-assign-operator
  #'#f)

(define-for-syntax (extract-assignment form proc)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    #:datum-literals (parsed group assignment left-hand-side)
    [(group (parsed (assignment e)) . tail) (values #'e #'tail)]
    [(group (parsed (left-hand-side ref set rhs-name)) ~! . tail)
     #:with assign::assign-op-seq #'tail
     #:do [(define op (attribute assign.op))]
     (build-assign op
                   #'assign.name
                   #'ref
                   #'set
                   #'rhs-name
                   #'assign.tail)]
    [_ (raise-result-error (proc-name proc) "Assign_Syntax" form)]))

(define-for-syntax (wrap-parsed stx)
  #`(parsed #,stx))

(define-for-syntax (make-assign-operator name prec protocol proc assc)
  (make-assign-infix-operator
   name
   prec
   assc
   protocol
   (if (eq? protocol 'macro)
       (lambda (ref set tail rhs-name)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (wrap-parsed #`(left-hand-side #,ref #,set #,rhs-name))
                                   (pack-tail #'tail #:after #'head)
                                   #'head)])))
         (define-values (ex-form more-tail)
           (extract-assignment form proc))
         (check-transformer-result ex-form
                                   (cond
                                     [(null? (syntax-e more-tail))
                                      (unpack-tail new-tail proc #f)]
                                     [(null? (syntax-e new-tail))
                                      (unpack-tail more-tail proc #f)]
                                     [else
                                      (unpack-tail (append (syntax->list more-tail) tail) proc #f)])
                                   proc))
       (lambda (ref set form2 stx rhs-name)
         (define-values (form tail)
           (extract-assignment (proc (wrap-parsed #`(left-hand-side #,ref #,set #,rhs-name))
                                     (wrap-parsed form2)
                                     stx)
                               proc))
         (unless (null? (syntax-e tail))
           (raise-syntax-error #f "expected empty tail" tail))
         form))))

(define-for-syntax (unpack_left stx)
  (syntax-parse (unpack-term stx 'assign_meta.unpack_left #f)
    #:datum-literals (parsed left-hand-side)
    [(parsed (left-hand-side ref set rhs-name))
     (values #'(parsed ref)
             #'(parsed set)
             #'rhs-name)]))

(define-for-syntax (pack_assignment stx)
  #`(parsed (assignment
             (rhombus-expression #,(unpack-group stx 'assign_meta.pack_expression #f)))))

(begin-for-syntax
  (define-syntax-class (:assign-parsed ref set rhs-name)
    #:attributes (parsed tail)
    #:datum-literals (group)
    (pattern (group . assign::assign-op-seq)
             #:do [(define op (attribute assign.op))
                   (define-values (assign-expr tail) (build-assign
                                                      op
                                                      #'assign.name
                                                      #`(rhombus-expression #,(unpack-group ref 'assign_meta.AssignParsed #f))
                                                      #`(rhombus-expression #,(unpack-group set 'assign_meta.AssignParsed #f))
                                                      (if (identifier? rhs-name)
                                                          rhs-name
                                                          (raise-argument-error 'assign_meta.AssignParsed
                                                                                "Identifier"
                                                                                rhs-name))
                                                      #'assign.tail))]
             #:attr parsed assign-expr
             #:attr tail tail))
  (define-syntax-class-syntax AssignParsed (make-syntax-class
                                            #':assign-parsed
                                            #:arity 8
                                            #:kind 'group
                                            #:fields #'((parsed #f parsed 0 unpack-parsed*)
                                                        (tail #f tail tail unpack-tail-list*))
                                            #:root-swap '(parsed . group))))
