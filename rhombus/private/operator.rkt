#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "operator-parse.rkt"
                     "consistent.rkt"
                     "srcloc.rkt")
         "expression.rkt"
         (only-in "repetition.rkt"
                  expression+repetition-prefix+infix-operator)
         "compound-repetition.rkt"
         "dotted-sequence-parse.rkt"
         "parse.rkt"
         "syntax.rkt"
         "definition.rkt"
         "static-info.rkt"
         (submod "function.rkt" for-build)
         (only-in "entry-point.rkt" no-adjustments))

;; The `operator` form takes something that looks like a function-style
;; operator definition and generates a combination of a transformer and
;; a function

(provide (rename-out [rhombus-operator operator]))

(begin-for-syntax
  
  (define-splicing-syntax-class :prefix-case
    (pattern (~seq (parens (~and g (group op-name-seq::dotted-operator-or-identifier-sequence arg)))
                   ret::ret-annotation
                   ((~and tag block) options::prefix-operator-options
                                     body ...))
             #:with op-name::dotted-operator-or-identifier #'op-name-seq
             #:attr name #'op-name.name
             #:attr prec #'options.prec
             #:attr rhs #'(tag body ...)
             #:attr ret-predicate #'ret.predicate
             #:attr ret-static-infos #'ret.static-infos))

  (define-splicing-syntax-class :infix-case
    (pattern (~seq (parens (~and g (group left op-name-seq::dotted-operator-or-identifier-sequence right)))
                   ret::ret-annotation
                   ((~and tag block) options::infix-operator-options
                                     body ...))
             #:with op-name::dotted-operator-or-identifier #'op-name-seq
             #:attr name #'op-name.name
             #:attr prec #'options.prec
             #:attr assc #'options.assc
             #:attr rhs #'(tag body ...)
             #:attr ret-predicate #'ret.predicate
             #:attr ret-static-infos #'ret.static-infos))

  (define (make-prefix name op-proc prec static-infos)
    (with-syntax ([op-proc op-proc])
      #`(make-expression&repetition-prefix-operator
         (quote-syntax #,name)
         #,(convert-prec prec)
         'automatic
         (lambda (arg self-stx)
           (relocate (span-srcloc self-stx arg)
                     (wrap-static-info*
                      #`(op-proc #,arg)
                      (quote-syntax #,static-infos)))))))

  (define (make-infix name op-proc prec assc static-infos)
    (with-syntax ([op-proc op-proc])
      #`(make-expression&repetition-infix-operator
         (quote-syntax #,name)
         #,(convert-prec prec)
         'automatic
         (lambda (left right self-stx)
           (relocate (span-srcloc left right)
                     (wrap-static-info*
                      #`(op-proc #,left #,right)
                      (quote-syntax #,static-infos))))
         #,(convert-assc assc))))

  (define (build-prefix-function name arg rhs start end ret-predicate)
    (syntax-parse #`(group #,arg)
      [arg::binding
       (build-function no-adjustments
                       name
                       #'(#f) #'(arg) #'(arg.parsed) #'(#f)
                       #'#f #'#f
                       #'#f #'#f
                       ret-predicate
                       rhs
                       start end)]))

  (define (build-infix-function name left right rhs start end ret-predicate)
    (syntax-parse #`(group #,left)
      [left::binding
       (syntax-parse #`(group #,right)
         [right::binding
          (build-function no-adjustments
                          name
                          #'(#f #f) #'(left right) #'(left.parsed right.parsed) #'(#f #f)
                          #'#f #'#f
                          #'#f #'#f
                          ret-predicate
                          rhs
                          start end)])]))

  (define (generate-prefix form-id g name arg prec rhs ret-predicate ret-static-infos)
    (with-syntax ([(op-proc) (generate-temporaries (list name))])
      (list
       #`(define op-proc
           #,(build-prefix-function name arg rhs form-id g ret-predicate))
       #`(define-syntax #,name
           #,(make-prefix name #'op-proc prec ret-static-infos)))))

  (define (generate-infix form-id g name left right prec assc rhs ret-predicate ret-static-infos)
    (with-syntax ([(op-proc) (generate-temporaries (list name))])
      (list
       #`(define op-proc
           #,(build-infix-function name left right rhs form-id g ret-predicate))
       #`(define-syntax #,name
           #,(make-infix name #'op-proc prec assc ret-static-infos)))))
    
  (define (generate-prefix+infix stx
                                 p-g p-name p-arg p-prec p-rhs p-ret-predicate p-ret-static-infos
                                 i-g i-name i-left i-right i-prec i-assc i-rhs i-ret-predicate i-ret-static-infos)
    (check-consistent stx (list i-name p-name) "operator name")
    (with-syntax ([(p-op-proc i-op-proc) (generate-temporaries (list i-name p-name))])
      (list
       #`(define p-op-proc
           #,(build-prefix-function p-name p-arg p-rhs p-g p-g p-ret-predicate))
       #`(define i-op-proc
           #,(build-infix-function i-name i-left i-right i-rhs i-g i-g i-ret-predicate))
       #`(define-syntax #,p-name
           (expression+repetition-prefix+infix-operator
            #,(make-prefix p-name #'p-op-proc p-prec p-ret-static-infos)
            #,(make-infix i-name #'i-op-proc i-prec i-assc i-ret-static-infos)))))))

(define-syntax rhombus-operator
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts op)
        [(form-id p::prefix-case)
         (generate-prefix #'form-id #'p.g #'p.name #'p.arg #'p.prec #'p.rhs
                          #'p.ret-predicate #'p.ret-static-infos)]
        [(form-id i::infix-case)
         (generate-infix #'form-id #'i.g #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs
                         #'i.ret-predicate #'i.ret-static-infos)]
        [(form-id (alts (block (group p::prefix-case))))
         (generate-prefix #'form-id #'p.g #'p.name #'p.arg #'p.prec #'p.rhs
                          #'p.ret-predicate #'p.ret-static-infos)]
        [(form-id (alts (block (group i::infix-case))))
         (generate-infix #'form-id #'i.g #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs
                         #'i.ret-predicate #'i.ret-static-infos)]
        [(form-id (alts (block (group p::prefix-case))
                        (block (group i::infix-case))))
         (generate-prefix+infix stx
                                #'p.g #'p.name #'p.arg #'p.prec #'p.rhs
                                #'p.ret-predicate #'p.ret-static-infos
                                #'i.g #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs
                                #'i.ret-predicate #'i.ret-static-infos)]
        [(form-id (alts (block (group i::infix-case))
                        (block (group p::prefix-case))))
         (generate-prefix+infix stx
                                #'p.g #'p.name #'p.arg #'p.prec #'p.rhs
                                #'p.ret-predicate #'p.ret-static-infos
                                #'i.g #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs
                                #'i.ret-predicate #'i.ret-static-infos)]))))
                                
