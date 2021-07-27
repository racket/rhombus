#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "op.rkt")
         "expression.rkt"
         "parse.rkt"
         (submod "function.rkt" for-call))

(provide #%block
         #%literal
         #%tuple
         #%call)

(define-syntax #%block
  (expression-prefix-operator
   #'%block
   '((default . stronger))
   #t ; transformer
   (lambda (stxes)
     (syntax-parse stxes
       [((~and head ((~datum block) . body)) . tail)
        (values #`(rhombus-block . body)
                #'tail)]))))


(define-syntax #%literal
  (expression-prefix-operator
   #'%literal
   '((default . stronger))
   #t ; transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(datum . tail)
        (values (syntax/loc #'datum (quote datum))
                #'tail)]))))

(define-syntax #%tuple
  (expression-prefix-operator
   #'%tuple
   '((default . stronger))
   #t ; transformer
   (lambda (stxes)
     (syntax-parse stxes
       [((~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty expression" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many expressions" #'head)]
            [else
             ;; eagerly parse content of parentheses; we could choose to
             ;; delay parsing by using `rhombus-expression`, instead
             (syntax-parse (car args)
               [e::expression (values #'e.expanded #'tail)])]))]))))

(define-syntax #%call
  (expression-infix-operator
   #'%call
   '((default . stronger))
   #t ; transformer
   (lambda (rator stxes)
     (parse-function-call rator stxes))
   'left))
