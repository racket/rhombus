#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     "consistent.rkt"
                     "srcloc.rkt")
         "expression.rkt"
         "quasiquote.rkt"
         "parse.rkt"
         "function.rkt"
         ;; to we generate compile-time code:
         (for-syntax "parse.rkt"))

(provide (for-syntax :syntax-quote
                     parse-operator-definition
                     parse-operator-definitions))

(begin-for-syntax
  (define-syntax-class :op/other
    #:datum-literals (op)
    (pattern (op name))
    (pattern (~and name (~literal other))))

  (define (combine-prec strongers weakers sames)
    (define ht (make-free-identifier-mapping))
    (define prec '())
    (define (add! op kind)
      (define old-op (free-identifier-mapping-get ht op (lambda () #f)))
      (when old-op
        (raise-syntax-error #f
                            "operator multiple times in precedence specifications"
                            op))
      (free-identifier-mapping-put! ht op op)
      (set! prec (cons (cons op kind) prec)))
    (for ([stronger (in-list strongers)])
      (add! stronger 'stronger))
    (for ([weaker (in-list weakers)])
      (add! weaker 'weaker))
    (for ([same (in-list sames)])
      (add! same 'same))
    (datum->syntax #f prec))
  
  (define-splicing-syntax-class :syntax-quote
    #:datum-literals (op parens group
                         stronger_than
                         weaker_than
                         same_as
                         associativity
                         right left none)
    #:literals (? :>)
    (pattern (~seq (op ?) (parens g))
             #:attr prec #'()
             #:attr assc #'#f)
    (pattern (~seq (op ?) (parens g
                                  (~alt (~optional (group stronger_than (op :>) stronger::op/other ...)
                                                   #:defaults ([(stronger.name 1) '()]))
                                        (~optional (group weaker_than (op :>) weaker::op/other ...)
                                                   #:defaults ([(weaker.name 1) '()]))
                                        (~optional (group same_as (op :>) same::op/other ...)
                                                   #:defaults ([(same.name 1) '()]))
                                        (~optional (group associativity (op :>) (~and assc
                                                                                      (~or right left none)))
                                                   #:defaults ([assc #'#f])))
                                  ...))
             #:attr prec (combine-prec (syntax->list #'(stronger.name ...))
                                       (syntax->list #'(weaker.name ...))
                                       (syntax->list #'(same.name ...))))))

(begin-for-syntax
  (struct prefix+infix (prefix infix)
    #:property prop:expression-prefix-operator (lambda (self) (prefix+infix-prefix self))
    #:property prop:expression-infix-operator (lambda (self) (prefix+infix-infix self))))

(define-for-syntax (parse-one-operator-definition g prec assc rhs)
  (define (convert-prec prec)
    #`(list #,@(for/list ([p (in-list (syntax->list prec))])
                 (syntax-parse p
                   [((~literal other) . spec) #`'(default . spec)]
                   [(op . spec) #`(cons (quote-syntax op) 'spec)]))))
  (syntax-parse g
    [((~datum group) ((~datum op) (~literal ¿)) left:identifier
                     ((~datum op) op)
                     ((~datum op) (~literal ¿)) right:identifier)
     #`(make-expression-infix-operator
        (quote-syntax op)
        #,(convert-prec prec)
        (let ([op (lambda (left right)
                    (rhombus-expression (group #,rhs)))])
          op)
        '#,(if (eq? (syntax-e assc) 'none)
               #'#f
               assc))]
    [((~datum group) ((~datum op) op)
                     ((~datum op) (~literal ¿)) arg:identifier)
     (when (syntax-e assc)
       (raise-syntax-error #f
                           "associatvity not allowed for infix operators"
                           assc))
     #`(make-expression-prefix-operator
        (quote-syntax op)
        #,(convert-prec prec)
        (let ([op (lambda (arg)
                    (rhombus-expression (group #,rhs)))])
          op))]))

(define-for-syntax (parse-operator-definition g prec assc rhs)
  (define p (parse-one-operator-definition g prec assc rhs))
  (define op (syntax-parse p [(_ (_ op) . _) #'op]))
  #`(define-syntax #,op #,p))

(define-for-syntax (parse-operator-definitions stx gs precs asscs rhss)
  (define ps (map parse-one-operator-definition gs precs asscs rhss))
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

(define-for-syntax (make-expression-infix-operator name prec proc assc)
  (expression-infix-operator
   name
   prec
   #f
   (lambda (form1 form2 stx)
     #`(rhombus-expression (group #,(check-expression-result
                                     (proc #`(parsed #,form1) #`(parsed #,form2))
                                     proc))))
   assc))

(define-for-syntax (make-expression-prefix-operator name prec proc)
  (expression-prefix-operator
   name
   prec
   #f
   (lambda (form stx)
     #`(rhombus-expression (group #,(check-expression-result
                                     (proc #`(parsed #,form))
                                     proc))))))
