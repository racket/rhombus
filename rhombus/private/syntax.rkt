#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     "transformer.rkt"
                     "consistent.rkt"
                     "srcloc.rkt")
         (rename-in "quasiquote.rkt"
                    [... rhombus...])
         "parse.rkt"
         "function.rkt"
         ;; because we generate compile-time code:
         (for-syntax "parse.rkt"))

(provide (for-syntax make-syntax-definition-transformer
                     
                     :operator-syntax-quote
                     :identifier-syntax-quote
                     parse-operator-definition
                     parse-operator-definitions
                     parse-transformer-definition))

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

  (define-syntax-class :operator-definition-group
    #:datum-literals (op group)
    #:literals (?)
    (pattern (group (op ¿) _ (op _) . _))
    (pattern (group (op _) . _)))

  (define-syntax-class :identifier-definition-group
    #:datum-literals (group)
    (pattern (group _:identifier . _)))

  (define-splicing-syntax-class :operator-syntax-quote
    #:datum-literals (op parens group
                         stronger_than
                         weaker_than
                         same_as
                         associativity
                         right left none)
    #:literals (? :>)
    (pattern (~seq (op ?) (parens g::operator-definition-group))
             #:attr prec #'()
             #:attr assc #'#f
             #:attr self-id #'self)
    (pattern (~seq (op ?) (parens g::operator-definition-group
                                  (~alt (~optional (group stronger_than (op :>) stronger::op/other ...)
                                                   #:defaults ([(stronger.name 1) '()]))
                                        (~optional (group weaker_than (op :>) weaker::op/other ...)
                                                   #:defaults ([(weaker.name 1) '()]))
                                        (~optional (group same_as (op :>) same::op/other ...)
                                                   #:defaults ([(same.name 1) '()]))
                                        (~optional (group associativity (op :>) (~and assc
                                                                                      (~or right left none)))
                                                   #:defaults ([assc #'#f]))
                                        (~optional (group op_stx (op :>) self-id:identifier)
                                                   #:defaults ([self-id #'self])))
                                  ...))
             #:attr prec (combine-prec (syntax->list #'(stronger.name ...))
                                       (syntax->list #'(weaker.name ...))
                                       (syntax->list #'(same.name ...)))))
  
  (define-splicing-syntax-class :identifier-syntax-quote
    #:datum-literals (op parens group)
    #:literals (? :>)
    (pattern (~seq (op ?) (parens g::identifier-definition-group
                                  (~optional (group op_stx (op :>) self-id:identifier)
                                             #:defaults ([self-id #'self])))))))

(define-for-syntax (parse-one-operator-definition make-prefix-id make-infix-id)
  (lambda (g prec assc self-id rhs)
    (define (convert-prec prec)
      #`(list #,@(for/list ([p (in-list (syntax->list prec))])
                   (syntax-parse p
                     [((~literal other) . spec) #`'(default . spec)]
                     [(op . spec) #`(cons (quote-syntax op) 'spec)]))))
    (syntax-parse g
      #:datum-literals (group op)
      #:literals (¿ rhombus...)
      [(group (op ¿) left:identifier
              (op op-name)
              (op ¿) right-or-tail:identifier
              (~optional (~and dots (op rhombus...))
                         #:defaults ([dots #'#f])))
       #`(#,make-infix-id
          (quote-syntax op-name)
          #,(convert-prec prec)
          (not (eq? (syntax-e #'dots) #f))
          (let ([op-name (lambda (left right-or-tail self-id)
                           (rhombus-expression (group #,rhs)))])
            op-name)
          '#,(if (eq? (syntax-e assc) 'none)
                 #'#f
                 assc))]
      [(group (op op-name)
              (op ¿) arg-or-tail:identifier
              (~optional (~and dots (op rhombus...))
                         #:defaults ([dots #'#f])))
       (when (syntax-e assc)
         (raise-syntax-error #f
                             "associativity not allowed for infix operators"
                             assc))
       #`(#,make-prefix-id
          (quote-syntax op-name)
          #,(convert-prec prec)
          (not (eq? (syntax-e #'dots) #f))
          (let ([op-name (lambda (arg-or-tail self-id)
                           (rhombus-expression (group #,rhs)))])
            op-name))])))

(define-for-syntax (parse-operator-definition g prec assc self-id rhs
                                              in-space make-prefix-id make-infix-id)
  (define p ((parse-one-operator-definition make-prefix-id make-infix-id) g prec assc self-id rhs))
  (define op (syntax-parse p [(_ (_ op) . _) #'op]))
  #`(define-syntax #,(in-space op) #,p))

(define-for-syntax (parse-operator-definitions stx gs precs asscs self-ids rhss
                                               in-space make-prefix-id make-infix-id prefix+infix-id)
  (define ps (map (parse-one-operator-definition make-prefix-id make-infix-id) gs precs asscs self-ids rhss))
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
                        "cannot handle multiple infix implementations"
                        (respan stx)))
  #`(define-syntax #,(in-space (car ops))
      #,(cond
          [(null? prefixes) (car infixes)]
          [(null? infixes) (car prefixes)]
          [else #`(#,prefix+infix-id #,(car prefixes) #,(car infixes))])))

(define-for-syntax (parse-transformer-definition g self-id rhs
                                                 in-space make-transformer-id)
  (syntax-parse g
    #:datum-literals (group op)
    #:literals (¿ rhombus...)
    [(group id:identifier
            (op ¿) tail:identifier
            (op rhombus...))
     #`(define-syntax #,(in-space #'id)
         (#,make-transformer-id
          (let ([id (lambda (tail self-id)
                      (rhombus-expression (group #,rhs)))])
            id)))]))

(define-for-syntax (make-syntax-definition-transformer in-space
                                                       make-transformer-id
                                                       make-prefix-id
                                                       make-infix-id
                                                       prefix+infix-id)
  (definition-transformer
    (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block alts op)
       [(form-id ((~and alts-tag alts) (block (group q::operator-syntax-quote
                                                     (~and rhs (block body ...))))
                                       ...+))
        (values
         (list (parse-operator-definitions stx
                                           (syntax->list #'(q.g ...))
                                           (syntax->list #'(q.prec ...))
                                           (syntax->list #'(q.assc ...))
                                           (syntax->list #'(q.self-id ...))
                                           (syntax->list #'(rhs ...))
                                           in-space
                                           make-prefix-id
                                           make-infix-id
                                           prefix+infix-id))
         null)]
       [(form-id q::operator-syntax-quote
                 (~and rhs (block body ...)))
        (values
         (list (parse-operator-definition #'q.g
                                          #'q.prec
                                          #'q.assc
                                          #'q.self-id
                                          #'rhs
                                          in-space
                                          make-prefix-id
                                          make-infix-id))
         null)]
       [(form-id q::identifier-syntax-quote
                 (~and rhs (block body ...)))
        (values
         (list (parse-transformer-definition #'q.g #'q.self-id #'rhs
                                             in-space make-transformer-id))
         null)]))))
