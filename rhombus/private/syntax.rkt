#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     "operator-parse.rkt"
                     "consistent.rkt"
                     "srcloc.rkt")
         (rename-in "quasiquote.rkt"
                    [... rhombus...])
         (submod "quasiquote.rkt" convert)
         "parse.rkt"
         "definition.rkt"
         "function.rkt"
         ;; because we generate compile-time code:
         (for-syntax "parse.rkt"))

(provide (for-syntax make-operator-definition-transformer
                     make-identifier-syntax-definition-transformer

                     parse-operator-definition
                     parse-operator-definitions
                     :operator-syntax-quote

                     :prefix-operator-options
                     :infix-operator-options
                     convert-prec
                     convert-assc))

(begin-for-syntax
  (define-syntax-class :op/other
    #:datum-literals (op)
    (pattern (op name))
    (pattern (~and name #:other)))

  (define (combine-prec strongers weakers sames same-on-lefts same-on-rights)
    (define ht (make-free-identifier-mapping))
    (define said-other? #f)
    (define prec '())
    (define (add! op kind)
      (cond
        [(eq? (syntax-e op) '#:other)
         (when said-other?
           (raise-syntax-error #f
                               "'other' multiple times in precedence specifications"
                               op))
         (set! said-other? #t)]
        [else
         (define old-op (free-identifier-mapping-get ht op (lambda () #f)))
         (when old-op
           (raise-syntax-error #f
                               "operator multiple times in precedence specifications"
                               op))
         (free-identifier-mapping-put! ht op op)])
      (set! prec (cons (cons op kind) prec)))
    (for ([stronger (in-list strongers)])
      (add! stronger 'stronger))
    (for ([weaker (in-list weakers)])
      (add! weaker 'weaker))
    (for ([same (in-list sames)])
      (add! same 'same))
    (for ([same (in-list same-on-lefts)])
      (add! same 'same-on-left))
    (for ([same (in-list same-on-rights)])
      (add! same 'same-on-right))
    (datum->syntax #f prec))

  (define-syntax-class :operator-definition-group
    #:datum-literals (op group)
    #:literals (?)
    (pattern (group (op ¿) _ _::operator . _))
    (pattern (group ::operator-or-identifier . _)))

  (define-splicing-syntax-class :operator-options
    #:datum-literals (op block group
                         stronger_than
                         weaker_than
                         same_as
                         same_on_left_as
                         same_on_right_as)
    (pattern (~seq (~alt (~optional (group #:stronger_than ~! (block (group stronger::op/other ...) ...))
                                    #:defaults ([(stronger.name 2) '()]))
                         (~optional (group #:weaker_than ~! (block (group weaker::op/other ...) ...))
                                    #:defaults ([(weaker.name 2) '()]))
                         (~optional (group #:same_as ~! (block (group same::op/other ...) ...))
                                    #:defaults ([(same.name 2) '()]))
                         (~optional (group #:same_on_left_as ~! (block (group same-on-left::op/other ...) ...))
                                    #:defaults ([(same-on-left.name 2) '()]))
                         (~optional (group #:same_on_right_as ~! (block (group same-on-right::op/other ...) ...))
                                    #:defaults ([(same-on-right.name 2) '()])))
                   ...)
             #:attr prec (combine-prec (syntax->list #'(stronger.name ... ...))
                                       (syntax->list #'(weaker.name ... ...))
                                       (syntax->list #'(same.name ... ...))
                                       (syntax->list #'(same-on-left.name ... ...))
                                       (syntax->list #'(same-on-right.name ... ...)))))

  (define-splicing-syntax-class :self-operator-options
    #:datum-literals (op block group
                         opt_stx)
    (pattern (~seq (~alt (~optional (group #:op_stx ~! (block (group self-id:identifier)))
                                    #:defaults ([self-id #'self])))
                   ...)))

  (define-splicing-syntax-class :prefix-operator-options
    (pattern (~seq opt::operator-options)
             #:attr prec #'opt.prec))
  
  (define-splicing-syntax-class :self-prefix-operator-options
    (pattern (~seq (~alt (~optional pre-opt::prefix-operator-options
                                    #:defaults ([pre-opt.prec #'()]))
                         (~optional self-opt::self-operator-options
                                    #:defaults ([self-opt.self-id #'self])))
                   ...)
             #:attr prec #'pre-opt.prec
             #:attr self-id #'self-opt.self-id))
             
  (define-splicing-syntax-class :infix-operator-options
    #:datum-literals (op block group
                         associativity)
    (pattern (~seq (~alt (~optional op-opt::operator-options
                                    #:defaults ([op-opt.prec #'()]))
                         (~optional (group #:associativity ~!
                                           (block (group (~and assc
                                                               (~or #:right #:left #:none)))))
                                    #:defaults ([assc #'#:left])))
                   ...)
             #:attr prec #'op-opt.prec))
             
  (define-splicing-syntax-class :self-infix-operator-options
    #:datum-literals (op parens group
                         stronger_than
                         weaker_than
                         same_as)
    (pattern (~seq (~alt (~optional in-opt::infix-operator-options
                                    #:defaults ([in-opt.prec #'()]
                                                [in-opt.assc #'none]))
                         (~optional self-opt::self-operator-options
                                    #:defaults ([self-opt.self-id #'self])))
                   ...)
             #:attr prec #'in-opt.prec
             #:attr assc #'in-opt.assc
             #:attr self-id #'self-opt.self-id))
             
  (define-splicing-syntax-class :operator-syntax-quote
    #:datum-literals (op parens group)
    #:literals (¿ ?)
    (pattern (~seq (op ?) (parens (~and g (group (op ¿) _ _::operator . _)))))
    (pattern (~seq (op ?) (parens (~and g (group ::operator-or-identifier . _))))))

  (define (convert-prec prec)
    #`(list #,@(for/list ([p (in-list (syntax->list prec))])
                 (syntax-parse p
                   [(#:other . spec) #`'(default . spec)]
                   [(op . spec) #`(cons (quote-syntax op) 'spec)]))))
  
  (define (convert-assc assc)
    #`'#,(string->symbol (keyword->string (syntax-e assc)))))

(define-for-syntax (parse-one-automatic-operator-definition make-prefix-id make-infix-id)
  (lambda (g rhs)
    (syntax-parse g
      #:datum-literals (group op)
      #:literals (¿ rhombus...)
      [(group (op ¿) left:identifier
              op-name::operator
              (op ¿) right:identifier)
       (syntax-parse rhs
         [((~and tag block) opt::self-infix-operator-options rhs ...)
          #`(#,make-infix-id
             (quote-syntax op-name.name)
             #,(convert-prec #'opt.prec)
             'automatic
             (let ([op-name.name (lambda (left right opt.self-id)
                                   (rhombus-expression (group (tag rhs ...))))])
               op-name.name)
             #,(convert-assc #'opt.assc))])]
      [(group op-name::operator-or-identifier
              (op ¿) arg:identifier)
       (syntax-parse rhs
         [((~and tag block) opt::self-prefix-operator-options rhs ...)
          #`(#,make-prefix-id
             (quote-syntax op-name.name)
             #,(convert-prec #'opt.prec)
             'automatic
             (let ([op-name.name (lambda (arg opt.self-id)
                                   (rhombus-expression (group (tag rhs ...))))])
               op-name.name))])])))

(define-for-syntax (parse-one-macro-operator-definition make-prefix-id make-infix-id)
  (lambda (g rhs)
    (define (macro-body self-id tail-id tail-pattern rhs)
      (define-values (pattern idrs can-be-empty?) (convert-pattern #`(parens (group . #,tail-pattern))))
      (with-syntax ([((id id-ref) ...) idrs])
        #`(syntax-parse (respan-empty #,self-id #,tail-id)
            [#,pattern
             (let ([id id-ref] ...)
               (rhombus-expression (group #,rhs)))])))
    (syntax-parse g
      #:datum-literals (group op)
      #:literals (¿ rhombus...)
      [(group (op ¿) left:identifier
              op-name::operator
              . tail-pattern)
       (syntax-parse rhs
         [((~and tag block) opt::self-infix-operator-options rhs ...)
          #`(#,make-infix-id
             (quote-syntax op-name.name)
             #,(convert-prec #'opt.prec)
             'macro
             (let ([op-name.name (lambda (left tail opt.self-id)
                                   #,(macro-body #'opt.self-id #'tail #'tail-pattern
                                                 #'(tag rhs ...)))])
               op-name.name)
             #,(convert-assc #'opt.assc))])]
      [(group op-name::operator-or-identifier
              . tail-pattern)
       (syntax-parse rhs
         [((~and tag block) opt::self-prefix-operator-options rhs ...)
          #`(#,make-prefix-id
             (quote-syntax op-name.name)
             #,(convert-prec #'opt.prec)
             'macro
             (let ([op-name.name (lambda (tail opt.self-id)
                                   #,(macro-body #'opt.self-id #'tail #'tail-pattern
                                                 #'(tag rhs ...)))])
               op-name.name))])])))

(define-for-syntax (parse-operator-definition protocol g rhs
                                              in-space make-prefix-id make-infix-id)
  (define p ((if (eq? protocol 'automatic)
                 (parse-one-automatic-operator-definition make-prefix-id make-infix-id)
                 (parse-one-macro-operator-definition make-prefix-id make-infix-id))
             g rhs))
  (define op (syntax-parse p [(_ (_ op-name) . _) #'op-name]))
  #`(define-syntax #,(in-space op) #,p))

(define-for-syntax (parse-operator-definitions protocol stx gs rhss
                                               in-space make-prefix-id make-infix-id prefix+infix-id)
  (define ps (map (if (eq? protocol 'automatic)
                      (parse-one-automatic-operator-definition make-prefix-id make-infix-id)
                      (parse-one-macro-operator-definition make-prefix-id make-infix-id))
                  gs rhss))
  (define-values (prefixes infixes ops)
    (let loop ([ps ps] [prefixes null] [infixes null] [ops null])
      (cond
        [(null? ps) (values (reverse prefixes) (reverse infixes) (reverse ops))]
        [else
         (syntax-parse (car ps)
           [(make (_ op-name) . _)
            #:when (free-identifier=? #'make make-prefix-id)
            (loop (cdr ps) (cons (car ps) prefixes) infixes (cons #'op-name ops))]
           [(make (_ op-name) . _)
            #:when (free-identifier=? #'make make-infix-id)
            (loop (cdr ps) prefixes (cons (car ps) infixes) (cons #'op-name ops))])])))
  (check-consistent stx ops "operator")
  (unless ((length prefixes) . < . 2)
    (raise-syntax-error #f
                        "cannot handle multiple prefix implementations"
                        stx))
  (unless ((length infixes) . < . 2)
    (raise-syntax-error #f
                        "cannot handle multiple infix implementations"
                        stx))
  #`(define-syntax #,(in-space (car ops))
      #,(cond
          [(null? prefixes) (car infixes)]
          [(null? infixes) (car prefixes)]
          [else #`(#,prefix+infix-id #,(car prefixes) #,(car infixes))])))

(define-for-syntax (make-operator-definition-transformer protocol
                                                         in-space
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
        (list (parse-operator-definitions protocol
                                          stx
                                          (syntax->list #'(q.g ...))
                                          (syntax->list #'(rhs ...))
                                          in-space
                                          make-prefix-id
                                          make-infix-id
                                          prefix+infix-id))]
       [(form-id q::operator-syntax-quote
                 (~and rhs (block body ...)))
        (list (parse-operator-definition protocol
                                         #'q.g
                                         #'rhs
                                         in-space
                                         make-prefix-id
                                         make-infix-id))]))))

;; ----------------------------------------

(begin-for-syntax
  (define-splicing-syntax-class :identifier-syntax-quote
    #:datum-literals (op parens group)
    #:literals (?)
    (pattern (~seq (op ?) (parens g::identifier-definition-group))))

  (define-syntax-class :identifier-definition-group
    #:datum-literals (group)
    (pattern (group _:identifier . _))))

(define-for-syntax (parse-transformer-definition g self-id rhs
                                                 in-space make-transformer-id)
  (syntax-parse g
    #:datum-literals (group op)
    #:literals (¿ rhombus...)
    [(group id:identifier . tail-pattern)
     (define-values (pattern idrs can-be-empty?) (convert-pattern #`(parens (group . tail-pattern))))
     (with-syntax ([((p-id id-ref) ...) idrs])
       #`(define-syntax #,(in-space #'id)
           (#,make-transformer-id
            (let ([id (lambda (tail #,self-id)
                        (syntax-parse (respan-empty #,self-id tail)
                          [#,pattern
                           (let ([p-id id-ref] ...)
                             (rhombus-expression (group #,rhs)))]))])
              id))))]))

(define-for-syntax (make-identifier-syntax-definition-transformer in-space
                                                                  make-transformer-id)
  (definition-transformer
    (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block alts op)
       [(form-id q::identifier-syntax-quote
                 (~and rhs (block
                            (~optional (group #:op_stx (block (group self-id:identifier)))
                                       #:defaults ([self-id #'self]))
                            body ...)))
        (list (parse-transformer-definition #'q.g #'self-id #'rhs
                                            in-space make-transformer-id))]))))
