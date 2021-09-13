#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     "operator-parse.rkt"
                     "consistent.rkt"
                     "srcloc.rkt"
                     "syntax-class-mixin.rkt")
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
                     make-identifier-syntax-definition-sequence-transformer

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
    (pattern (group (op ¿) _ _::operator-or-identifier . _))
    (pattern (group ::operator-or-identifier . _)))

  (define-syntax-class-mixin operator-options
    #:datum-literals (op block group
                         stronger_than
                         weaker_than
                         same_as
                         same_on_left_as
                         same_on_right_as)
    (~alt (~optional (group #:stronger_than ~! (block (group stronger::op/other ...) ...))
                     #:defaults ([(stronger.name 2) '()]))
          (~optional (group #:weaker_than ~! (block (group weaker::op/other ...) ...))
                     #:defaults ([(weaker.name 2) '()]))
          (~optional (group #:same_as ~! (block (group same::op/other ...) ...))
                     #:defaults ([(same.name 2) '()]))
          (~optional (group #:same_on_left_as ~! (block (group same-on-left::op/other ...) ...))
                     #:defaults ([(same-on-left.name 2) '()]))
          (~optional (group #:same_on_right_as ~! (block (group same-on-right::op/other ...) ...))
                     #:defaults ([(same-on-right.name 2) '()])))
    #:attr prec (combine-prec (syntax->list #'(stronger.name ... ...))
                              (syntax->list #'(weaker.name ... ...))
                              (syntax->list #'(same.name ... ...))
                              (syntax->list #'(same-on-left.name ... ...))
                              (syntax->list #'(same-on-right.name ... ...))))

  (define-syntax-class-mixin self-options
    #:datum-literals (op block group
                         opt_stx)
    (~alt (~optional (group #:op_stx ~! (block (group self-id:identifier)))
                     #:defaults ([self-id #'self]))))

  (define-syntax-class-mixin parsed-right-options
    #:datum-literals (op block group
                         opt_stx)
    (~alt (~optional (group (~and parsed-right? #:parsed_right))
                     #:defaults ([parsed-right? #'#f]))))

  (define-composed-splicing-syntax-class :prefix-operator-options
    operator-options)

  (define-composed-splicing-syntax-class :self-prefix-operator-options
    operator-options
    self-options)
  
  (define-composed-splicing-syntax-class :macro-prefix-operator-options
    operator-options
    self-options
    parsed-right-options)

  (define-syntax-class-mixin infix-operator-options
    #:datum-literals (op block group
                         associativity)
    (~alt (~optional (group #:associativity ~!
                            (block (group (~and assc
                                                (~or #:right #:left #:none)))))
                     #:defaults ([assc #'#:left]))))

  (define-composed-splicing-syntax-class :infix-operator-options
    operator-options
    infix-operator-options)
             
  (define-composed-splicing-syntax-class :macro-infix-operator-options
    operator-options
    infix-operator-options
    self-options
    parsed-right-options)

  (define-splicing-syntax-class :operator-syntax-quote
    #:datum-literals (op parens group)
    #:literals (¿ ?)
    (pattern (~seq (op ?) (parens (~and g (group (op ¿) _ _::operator-or-identifier . _)))))
    (pattern (~seq (op ?) (parens (~and g (group ::operator-or-identifier . _)))))
    (pattern (~seq (op ?) g::operator-or-identifier)))

  (define (convert-prec prec)
    #`(list #,@(for/list ([p (in-list (syntax->list prec))])
                 (syntax-parse p
                   [(#:other . spec) #`'(default . spec)]
                   [(op . spec) #`(cons (quote-syntax op) 'spec)]))))
  
  (define (convert-assc assc)
    #`'#,(string->symbol (keyword->string (syntax-e assc)))))

(define-for-syntax (parse-one-macro-definition kind make-prefix-id make-infix-id)
  (lambda (g rhs)
    (define (macro-body self-id left-ids tail-id tail-pattern rhs)
      (define-values (pattern idrs can-be-empty?)
        (if (eq? kind 'rule)
            (convert-pattern #`(parens (group #,@tail-pattern (op ¿) tail (op ......))))
            (convert-pattern #`(parens (group . #,tail-pattern)))))
      (with-syntax ([((id id-ref) ...) idrs])
        (define body
          (if (eq? kind 'rule)
              (let ([ids (cons self-id (append left-ids (syntax->list #'(id ...))))])
                #`(values #,(convert-rule-template rhs ids) tail))
              #`(rhombus-expression (group #,rhs))))
        #`(syntax-parse (respan-empty #,self-id #,tail-id)
            [#,pattern
             (let ([id id-ref] ...)
               #,body)])))
    (define (convert-rule-template block ids)
      (syntax-parse block
        #:datum-literals (block group op)
        #:literals (?)
        [(block (group (op (~and t-op ?)) template))
         (convert-template #'template
                           #:rhombus-expression #'rhombus-expression
                           #:check-escape (lambda (e)
                                            (unless (and (identifier? e)
                                                         (for/or ([id (in-list ids)])
                                                           (free-identifier=? e id)))
                                              (raise-syntax-error #f
                                                                  "expected an identifier bound by the pattern"
                                                                  #'t-op
                                                                  e))))]))
    (define (extract-pattern-id tail-pattern)
      (syntax-parse tail-pattern
        #:datum-literals (op)
        #:literals (¿)
        [((op ¿) id:identifier) #'id]))
    (syntax-parse g
      #:datum-literals (group op)
      #:literals (¿ rhombus...)
      ;; infix protocol
      [(group (op ¿) left:identifier
              op-name::operator-or-identifier
              . tail-pattern)
       (syntax-parse rhs
         [((~and tag block) opt::macro-infix-operator-options rhs ...)
          #`(#,make-infix-id
             (quote-syntax op-name.name)
             #,(convert-prec #'opt.prec)
             #,(if (syntax-e #'opt.parsed-right?)
                   #''automatic
                   #''macro)
             #,(cond
                 [(syntax-e #'opt.parsed-right?)
                  (define right-id (extract-pattern-id #'tail-pattern))
                  #`(let ([op-name.name (lambda (left #,right-id opt.self-id)
                                          #,(if (eq? kind 'rule)
                                                (convert-rule-template #'(tag rhs ...)
                                                                       (list #'left right-id #'opt.self-id))
                                                #`(rhombus-expression (group (tag rhs ...)))))])
                      op-name.name)]
                 [else
                  #`(let ([op-name.name (lambda (left tail opt.self-id)
                                          #,(macro-body #'opt.self-id (list #'left)
                                                        #'tail #'tail-pattern
                                                        #'(tag rhs ...)))])
                      op-name.name)])
             #,(convert-assc #'opt.assc))])]
      ;; prefix protocol
      [(group op-name::operator-or-identifier
              . tail-pattern)
       (syntax-parse rhs
         [((~and tag block) opt::macro-prefix-operator-options rhs ...)
          #`(#,make-prefix-id
             (quote-syntax op-name.name)
             #,(convert-prec #'opt.prec)
             #,(if (syntax-e #'opt.parsed-right?)
                   #''automatic
                   #''macro)
             #,(cond
                 [(syntax-e #'opt.parsed-right?)
                  (define arg-id (extract-pattern-id #'tail-pattern))
                  #`(let ([op-name.name (lambda (#,arg-id opt.self-id)
                                          #,(if (eq? kind 'rule)
                                                (convert-rule-template #'(tag rhs ...)
                                                                       (list arg-id #'opt-self-id))
                                                #`(rhombus-expression (group (tag rhs ...)))))])
                      op-name.name)]
                 [else
                  #`(let ([op-name.name (lambda (tail opt.self-id)
                                          #,(macro-body #'opt.self-id '()
                                                        #'tail #'tail-pattern
                                                        #'(tag rhs ...)))])
                      op-name.name)]))])]
      ;; nofix protocol
      [op-name::operator-or-identifier
       (syntax-parse rhs
         [((~and tag block) opt::self-prefix-operator-options rhs ...)
          #`(#,make-prefix-id
             (quote-syntax op-name.name)
             #,(convert-prec #'opt.prec)
             'macro
             (let ([op-name.name (lambda (tail opt.self-id)
                                   (values #,(if (eq? kind 'rule)
                                                 (convert-rule-template #'(tag rhs ...)
                                                                        (list #'opt.self-id))
                                                 #`(rhombus-block-at tag rhs ...))
                                           tail))])
               op-name.name))])])))

(define-for-syntax (parse-operator-definition kind g rhs
                                              in-space make-prefix-id make-infix-id)
  (define p ((parse-one-macro-definition kind make-prefix-id make-infix-id) g rhs))
  (define op (syntax-parse p [(_ (_ op-name) . _) #'op-name]))
  #`(define-syntax #,(in-space op) #,p))

(define-for-syntax (parse-operator-definitions kind stx gs rhss
                                               in-space make-prefix-id make-infix-id prefix+infix-id)
  (define ps (map (parse-one-macro-definition kind make-prefix-id make-infix-id)
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
      (define (template->macro protocol) (if (eq? protocol 'template) 'macro protocol))
      (syntax-parse stx
        #:datum-literals (parens group block alts op)
        [(form-id ((~and alts-tag alts) (block (group q::operator-syntax-quote
                                                      (~and rhs (block body ...))))
                                        ...+))
         (list (parse-operator-definitions (template->macro protocol)
                                           stx
                                           (syntax->list #'(q.g ...))
                                           (syntax->list #'(rhs ...))
                                           in-space
                                           make-prefix-id
                                           make-infix-id
                                           prefix+infix-id))]
        [(form-id q::operator-syntax-quote
                  (~and rhs (block body ...)))
         (list (parse-operator-definition (template->macro protocol)
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
    (pattern (group _:identifier . _)))
  
  (define-splicing-syntax-class :identifier-sequence-syntax-quote
    #:datum-literals (op block parens group)
    #:literals (?)
    (pattern (~seq (op ?) (parens (group (block g::identifier-definition-group
                                                . gs)))))))

(define-for-syntax (parse-transformer-definition g self-id rhs
                                                 in-space make-transformer-id
                                                 #:tail-ids [tail-ids '()]
                                                 #:wrap-for-tail [wrap-for-tail values])
  (syntax-parse g
    #:datum-literals (group op)
    #:literals (¿ rhombus...)
    [(group id:identifier . tail-pattern)
     (define-values (pattern idrs can-be-empty?) (convert-pattern #`(parens (group . tail-pattern))))
     (with-syntax ([((p-id id-ref) ...) idrs])
       #`(define-syntax #,(in-space #'id)
           (#,make-transformer-id
            (let ([id (lambda (tail #,@tail-ids #,self-id)
                        (syntax-parse (respan-empty #,self-id tail)
                          [#,pattern
                           (let ([p-id id-ref] ...)
                             #,(wrap-for-tail
                                #`(rhombus-expression (group #,rhs))))]))])
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

(define-for-syntax (make-identifier-syntax-definition-sequence-transformer in-space
                                                                           make-transformer-id)
  (definition-transformer
    (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block alts op)
       [(form-id q::identifier-sequence-syntax-quote
                 (~and rhs (block
                            (~optional (group #:op_stx (block (group self-id:identifier)))
                                       #:defaults ([self-id #'self]))
                            body ...)))
        (list (parse-transformer-definition #'q.g #'self-id #'rhs
                                            in-space make-transformer-id
                                            #:tail-ids #'(tail-id)
                                            #:wrap-for-tail
                                            (lambda (body)
                                              (define-values (pattern idrs can-be-empty?) (convert-pattern #`(parens (group (block . q.gs)))))
                                              (with-syntax ([((p-id id-ref) ...) idrs])
                                                #`(syntax-parse tail-id
                                                    [#,pattern
                                                     (let ([p-id id-ref] ...)
                                                       #,body)])))))]))))
