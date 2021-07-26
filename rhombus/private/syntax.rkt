#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "consistent.rkt"
                     "srcloc.rkt")
         "expression.rkt"
         "quasiquote.rkt"
         "parse.rkt"
         ;; to we generate compile-time code:
         (for-syntax "parse.rkt"))

(provide (for-syntax parse-operator-definition
                     parse-operator-definitions))

(begin-for-syntax
  (struct prefix+infix (prefix infix)
    #:property prop:expression-prefix-operator (lambda (self) (prefix+infix-prefix self))
    #:property prop:expression-infix-operator (lambda (self) (prefix+infix-infix self))))

(define-for-syntax (parse-one-operator-definition g rhs)
  (syntax-parse g
    [((~datum group) ((~datum op) (~literal ¿)) left:identifier
                     ((~datum op) op)
                     ((~datum op) (~literal ¿)) right:identifier)
     #`(make-expression-infix-operator
        (quote-syntax op)
        (let ([op (lambda (left right)
                    (rhombus-expression (group #,rhs)))])
          op))]
    [((~datum group) ((~datum op) op)
                     ((~datum op) (~literal ¿)) arg:identifier)
     #`(make-expression-prefix-operator
        (quote-syntax op)
        (let ([op (lambda (arg)
                    (rhombus-expression (group #,rhs)))])
          op))]))

(define-for-syntax (parse-operator-definition g rhs)
  (define p (parse-one-operator-definition g rhs))
  (define op (syntax-parse p [(_ (_ op) . _) #'op]))
  #`(define-syntax #,op #,p))

(define-for-syntax (parse-operator-definitions stx gs rhss)
  (define ps (map parse-one-operator-definition gs rhss))
  (define-values (prefixes infixes ops)
    (let loop ([ps ps] [prefixes null] [infixes null] [ops null])
      (cond
        [(null? ps) (values (reverse prefixes) (reverse infixes) (reverse ops))]
        [else
         (syntax-parse (car ps)
           [((~literal make-expression-prefix-operator) (_ op) . _)
            (loop (cdr ps) (cons (car ps) prefixes) infixes (cons #'op ops))]
           [((~literal make-expression-infix-operator) (_ op) . _)
            (loop (cdr ps) prefixes (cons (car ps) infixes) (cons #'op ops))])])))
  (check-consistent stx ops "operator")
  (unless ((length prefixes) . < . 2)
    (raise-syntax-error #f
                        "cannot handle multiple prefix implementations"
                        (respan stx)))
  (unless ((length infixes) . < . 2)
    (raise-syntax-error #f
                        "cannot handle two infix implementations"
                        (respan stx)))
  #`(define-syntax #,(car ops)
      #,(cond
          [(null? prefixes) (car infixes)]
          [(null? infixes) (car prefixes)]
          [else #`(prefix+infix #,(car prefixes) #,(car infixes))])))

(define-for-syntax (make-expression-infix-operator name proc)
  (expression-infix-operator
   name
   '()
   #f
   (lambda (form1 form2 stx)
     #`(rhombus-expression (group #,(check-expression-result
                                     (proc #`(parsed #,form1) #`(parsed #,form2))
                                     proc))))
   #f))

(define-for-syntax (make-expression-prefix-operator name proc)
  (expression-prefix-operator
   name
   '()
   #f
   (lambda (form stx)
     #`(rhombus-expression (group #,(check-expression-result
                                     (proc #`(parsed #,form))
                                     proc))))))
