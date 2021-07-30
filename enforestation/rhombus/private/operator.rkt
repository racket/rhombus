#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/operator-parse
                     "consistent.rkt")
         "expression.rkt"
         "parse.rkt"
         "syntax.rkt"
         "definition.rkt")

;; The `operator` form takes something that looks like a function-style
;; operator definition and generates a combination of a transformer and
;; a function

(provide (rename-out [rhombus-operator operator]))

(begin-for-syntax
  
  (define-splicing-syntax-class :prefix-case
    (pattern (~seq (parens (group op-name::operator arg:identifier)
                           options::prefix-operator-options)
                   (~and rhs (block body ...)))
             #:attr name #'op-name.name
             #:attr prec #'options.prec))

  (define-splicing-syntax-class :infix-case
    (pattern (~seq (~and (parens (group left:identifier op-name::operator right:identifier)
                                 options::infix-operator-options))
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

  (define (generate-prefix name arg prec rhs)
    (with-syntax ([(op-proc) (generate-temporaries (list name))])
      (list
       #`(define op-proc (lambda (#,arg)
                           (rhombus-expression (group #,rhs))))
       #`(define-syntax #,name
           #,(make-prefix name #'op-proc prec)))))

  (define (generate-infix name left right prec assc rhs)
    (with-syntax ([(op-proc) (generate-temporaries (list name))])
      (list
       #`(define op-proc (lambda (#,left #,right)
                           (rhombus-expression (group #,rhs))))
       #`(define-syntax #,name
           #,(make-infix name #'op-proc prec assc)))))
    
  (define (generate-prefix+infix stx
                                 p-name p-arg p-prec p-rhs
                                 i-name i-left i-right i-prec i-assc i-rhs)
    (check-consistent stx (list i-name p-name) "operator name")
    (with-syntax ([(p-op-proc i-op-proc) (generate-temporaries (list i-name p-name))])
      (list
       #`(define p-op-proc (lambda (#,p-arg)
                             (rhombus-expression (group #,p-rhs))))
       #`(define i-op-proc (lambda (#,i-left #,i-right)
                             (rhombus-expression (group #,i-rhs))))
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
         (generate-prefix #'p.name #'p.arg #'p.prec #'p.rhs)]
        [(form-id i::infix-case)
         (generate-infix #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs)]
        [(form-id (alts (block (group p::prefix-case))))
         (generate-prefix #'p.name #'p.arg #'p.prec #'p.rhs)]
        [(form-id (alts (block (group i::infix-case))))
         (generate-infix #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs)]
        [(form-id (alts (block (group p::prefix-case))
                        (block (group i::infix-case))))
         (generate-prefix+infix stx
                                #'p.name #'p.arg #'p.prec #'p.rhs
                                #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs)]
        [(form-id (alts (block (group i::infix-case))
                        (block (group p::prefix-case))))
         (generate-prefix+infix stx
                                #'p.name #'p.arg #'p.prec #'p.rhs
                                #'i.name #'i.left #'i.right #'i.prec #'i.assc #'i.rhs)]))))
                                
