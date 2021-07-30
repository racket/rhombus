#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/operator-parse
                     "consistent.rkt")
         "expression.rkt"
         "parse.rkt"
         "syntax.rkt"
         "definition.rkt"
         (submod "function.rkt" for-build))

;; The `operator` form takes something that looks like a function-style
;; operator definition and generates a combination of a transformer and
;; a function

(provide (rename-out [rhombus-operator operator]))

(begin-for-syntax
  
  (define-splicing-syntax-class :prefix-case
    (pattern (~seq (parens (~and g (group op-name::operator arg))
                           options::prefix-operator-options)
                   (~and rhs (block body ...)))
             #:attr name #'op-name.name
             #:attr prec #'options.prec))

  (define-splicing-syntax-class :infix-case
    (pattern (~seq (~and (~and g (parens (group left op-name::operator right)
                                         options::infix-operator-options)))
                   (~and rhs (block body ...)))
             #:attr name #'op-name.name
             #:attr prec #'options.prec
             #:attr assc #'options.assc))

  (define (make-prefix name op-proc prec)
    (with-syntax ([op-proc op-proc])
      #`(expression-prefix-operator
         (quote-syntax #,name)
         #,(convert-prec prec)
         'automatic
         (lambda (arg self-stx)
           #`(op-proc #,arg)))))

  (define (make-infix name op-proc prec assc)
    (with-syntax ([op-proc op-proc])
      #`(expression-infix-operator
         (quote-syntax #,name)
         #,(convert-prec prec)
         'automatic
         (lambda (left right self-stx)
           #`(op-proc #,left #,right))
         #,(convert-assc assc))))

  (define (build-prefix-function name arg rhs start end)
    (syntax-parse #`(group #,arg)
      [arg::binding
       (build-function name #'(#f) #'(arg) #'(arg.parsed) #'(#f) rhs start end)]))

  (define (build-infix-function name left right rhs start end)
    (syntax-parse #`(group #,left)
      [left::binding
       (syntax-parse #`(group #,right)
         [right::binding
          (build-function name
                          #'(#f #f) #'(left right) #'(left.parsed right.parsed) #'(#f #f)
                          rhs start end)])]))

  (define (generate-prefix form-id g name arg prec rhs)
    (with-syntax ([(op-proc) (generate-temporaries (list name))])
      (list
       #`(define op-proc
           #,(build-prefix-function name arg rhs form-id g))
       #`(define-syntax #,name
           #,(make-prefix name #'op-proc prec)))))

  (define (generate-infix form-id g name left right prec assc rhs)
    (with-syntax ([(op-proc) (generate-temporaries (list name))])
      (list
       #`(define op-proc
           #,(build-infix-function name left right rhs form-id g))
       #`(define-syntax #,name
           #,(make-infix name #'op-proc prec assc)))))
    
  (define (generate-prefix+infix stx
                                 p-g p-name p-arg p-prec p-rhs
                                 i-g i-name i-left i-right i-prec i-assc i-rhs)
    (check-consistent stx (list i-name p-name) "operator name")
    (with-syntax ([(p-op-proc i-op-proc) (generate-temporaries (list i-name p-name))])
      (list
       #`(define p-op-proc
           #,(build-prefix-function p-name p-arg p-rhs p-g p-g))
       #`(define i-op-proc
           #,(build-infix-function i-name i-left i-right i-rhs i-g i-g))
       #`(define-syntax #,p-name
           (expression-prefix+infix-operator
            #,(make-prefix p-name #'p-op-proc p-prec)
            #,(make-infix i-name #'i-op-proc i-prec i-assc)))))))

(define-syntax rhombus-operator
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts op)
        [(form-id p::prefix-case)
         (generate-prefix #'form-id #'p.g #'p.name #'p.arg #'p.prec #'p.rhs)]
        [(form-id i::infix-case)
         (generate-infix #'form-id #'i.g #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs)]
        [(form-id (alts (block (group p::prefix-case))))
         (generate-prefix #'form-id #'p.g #'p.name #'p.arg #'p.prec #'p.rhs)]
        [(form-id (alts (block (group i::infix-case))))
         (generate-infix #'form-id #'i.g #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs)]
        [(form-id (alts (block (group p::prefix-case))
                        (block (group i::infix-case))))
         (generate-prefix+infix stx
                                #'p.g #'p.name #'p.arg #'p.prec #'p.rhs
                                #'i.g #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs)]
        [(form-id (alts (block (group i::infix-case))
                        (block (group p::prefix-case))))
         (generate-prefix+infix stx
                                #'p.g #'p.name #'p.arg #'p.prec #'p.rhs
                                #'i.g #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs)]))))
                                
