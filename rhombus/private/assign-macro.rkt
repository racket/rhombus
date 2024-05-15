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
                     "macro-result.rkt"
                     "realm.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     "values-key.rkt"
                     (for-syntax racket/base))
         "name-root.rkt"
         "macro-macro.rkt"
         "parse.rkt"
         (submod "assign.rkt" for-assign))

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
    ([unpack_left assign_meta.unpack_left]
     [pack_assignment assign_meta.pack_assignment]
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
    [(group (parsed #:rhombus/assign (assignment e)) . tail) (values #'e #'tail)]
    [(group (parsed #:rhombus/assign (left-hand-side ref set rhs-name)) ~! . tail)
     #:with assign::assign-op-seq #'tail
     #:do [(define op (attribute assign.op))]
     (build-assign op
                   #'assign.op-name
                   #'assign.name
                   #'ref
                   #'set
                   #'rhs-name
                   #'assign.tail)]
    [_ (raise-bad-macro-result (proc-name proc) "assignment" form)]))

(define-for-syntax (make-assign-operator prec protocol proc assc)
  (make-assign-infix-operator
   prec
   assc
   protocol
   (if (eq? protocol 'macro)
       (lambda (ref set tail rhs-name)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc #`(parsed #:rhombus/assign (left-hand-side #,ref #,set #,rhs-name))
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
           (extract-assignment (proc #`(parsed #:rhombus/assign (left-hand-side #,ref #,set #,rhs-name))
                                     #`(parsed #:rhombus/expr #,form2)
                                     stx)
                               proc))
         (unless (null? (syntax-e tail))
           (raise-syntax-error #f "expected empty tail" tail))
         form))))

(define-for-syntax (check-syntax who s)
  (unless (syntax? s)
    (raise-argument-error* who rhombus-realm "Syntax" s)))

(begin-for-syntax
  (define/arity (assign_meta.unpack_left stx)
    #:static-infos ((#%call-result ((#%values (#,syntax-static-infos
                                               #,syntax-static-infos
                                               #,syntax-static-infos)))))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed left-hand-side)
      [(parsed #:rhombus/assign (left-hand-side ref set rhs-name))
       (values #'(parsed #:rhombus/expr ref)
               #'(parsed #:rhombus/expr set)
               #'rhs-name)]))

  (define/arity (assign_meta.pack_assignment stx)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (check-syntax who stx)
    #`(parsed #:rhombus/assign
              (assignment
               (rhombus-expression #,(unpack-group stx who #f)))))

  (define-syntax-class (:assign-parsed ref set rhs-name)
    #:attributes (parsed tail)
    #:datum-literals (group)
    (pattern (group . assign::assign-op-seq)
             #:do [(define op (attribute assign.op))
                   (define-values (assign-expr tail)
                     (build-assign
                      op
                      #'assign.op-name
                      #'assign.name
                      #`(rhombus-expression #,(unpack-group ref 'assign_meta.AssignParsed #f))
                      #`(rhombus-expression #,(unpack-group set 'assign_meta.AssignParsed #f))
                      (if (identifier? rhs-name)
                          rhs-name
                          (raise-argument-error 'assign_meta.AssignParsed
                                                "Identifier"
                                                rhs-name))
                      #'assign.tail))]
             #:with parsed assign-expr
             #:with tail tail))
  (define-syntax-class-syntax AssignParsed
    (make-syntax-class
     #':assign-parsed
     #:arity 8
     #:kind 'group
     #:fields #'((parsed #f parsed 0 (unpack-parsed* '#:rhombus/expr))
                 (tail #f tail tail unpack-tail-list*))
     #:root-swap '(parsed . group)))
  )
