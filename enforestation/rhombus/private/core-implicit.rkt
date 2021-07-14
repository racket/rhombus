#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "op.rkt")
         "parse.rkt")

(provide #%block
         #%literal
         #%tuple
         #%call)

(define-syntax #%block
  (rhombus-prefix-operator-transformer
   #'%block
   '((default . stronger))
   (lambda (stxes)
     (syntax-parse stxes
       [((~and head ((~datum block) . body)) . tail)
        (values #`(rhombus-block . body)
                #'tail)]))))


(define-syntax #%literal
  (rhombus-prefix-operator-transformer
   #'%literal
   '((default . stronger))
   (lambda (stxes)
     (syntax-parse stxes
       [(datum . tail)
        (values (syntax/loc #'datum (quote datum))
                #'tail)]))))

(define-syntax #%tuple
  (rhombus-prefix-operator-transformer
   #'%tuple
   '((default . stronger))
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
  (rhombus-infix-operator-transformer
   #'%call
   '((default . stronger))
   'left
   (lambda (rator stxes)
     (syntax-parse stxes
       [(((~and head (~datum parens)) rand::expression ...) . tail)
        (values (datum->syntax (quote-syntax here)
                               (cons rator #'(rand.expanded ...))
                               (span-srcloc rator #'head)
                               #'head)
                #'tail)]))))
