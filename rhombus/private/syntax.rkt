#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     "operator-parse.rkt"
                     "consistent.rkt"
                     "syntax-class-mixin.rkt"
                     "syntax-rhs.rkt"
                     (for-syntax racket/base
                                 syntax/parse))
         "parse.rkt"
         "definition.rkt"
         "function.rkt"
         (only-in "quasiquote.rkt" $)
         ;; because we generate compile-time code:
         (for-syntax "parse.rkt"))

(provide define-operator-definition-transformer
         define-identifier-syntax-definition-transformer
         define-identifier-syntax-definition-sequence-transformer
         
         (for-syntax parse-operator-definition
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
    (pattern name:identifier)
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
    (pattern (group (op $) _ _::operator-or-identifier . _))
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
    #:datum-literals (op block group)
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
    #:datum-literals (op parens group quotes)
    (pattern (quotes (~and g (group (op $) _ _::operator-or-identifier . _))))
    (pattern (quotes (~and g (group ::operator-or-identifier . _)))))

  (define (convert-prec prec)
    #`(list #,@(for/list ([p (in-list (syntax->list prec))])
                 (syntax-parse p
                   [(#:other . spec) #`'(default . spec)]
                   [(op . spec) #`(cons (quote-syntax op) 'spec)]))))

  (define (convert-assc assc)
    #`'#,(string->symbol (keyword->string (syntax-e assc)))))

;; parse one case (possibly the only case) in a macro definition
(define-for-syntax (parse-one-macro-definition kind)
  (lambda (g rhs)
    (syntax-parse g
      #:datum-literals (group op)
      ;; infix protocol
      [(group (op $-id) left:identifier
              op-name::operator-or-identifier
              . tail-pattern)
       #:when (free-identifier=? #'$-id #'$
                                 (add1 (syntax-local-phase-level))
                                 (syntax-local-phase-level))
       (syntax-parse rhs
         [((~and tag block) opt::macro-infix-operator-options rhs ...)
          #`(pre-parsed op-name.name
                        infix
                        #,kind
                        opt
                        #,(convert-prec #'opt.prec)
                        #,(convert-assc #'opt.assc)
                        opt.parsed-right?
                        [tail-pattern
                         opt.self-id
                         left
                         (tag rhs ...)])])]
      ;; prefix protocol
      [(group op-name::operator-or-identifier
              . tail-pattern)
       (syntax-parse rhs
         [((~and tag block) opt::macro-prefix-operator-options rhs ...)
          #`(pre-parsed op-name.name
                        prefix
                        #,kind
                        opt
                        #,(convert-prec #'opt.prec)
                        #f
                        opt.parsed-right?
                        [tail-pattern
                         opt.self-id
                         (tag rhs ...)])])]
      ;; nofix protocol
      [op-name::operator-or-identifier
       (syntax-parse rhs
         [((~and tag block) opt::self-prefix-operator-options rhs ...)
          #`(pre-parsed op-name.name
                        nofix
                        #,kind
                        opt
                        #,(convert-prec #'opt.prec)
                        #f
                        #f
                        [opt.self-id
                         (tag rhs ...)])])])))

(define-for-syntax (pre-parsed-name pre-parsed)
  (syntax-parse pre-parsed
    [(_ name . _) #'name]))

;; single-case macro definition:
(define-for-syntax (parse-operator-definition kind g rhs in-space compiletime-id)
  (define p ((parse-one-macro-definition kind) g rhs))
  (define op (pre-parsed-name p))
  #`(define-syntax #,(in-space op) (#,compiletime-id #,p)))

;; multi-case macro definition:
(define-for-syntax (parse-operator-definitions kind stx gs rhss in-space compiletime-id)
  (define ps (map (parse-one-macro-definition kind)
                  gs rhss))
  (check-consistent stx (map pre-parsed-name ps) "operator")
  #`(define-syntax #,(in-space (pre-parsed-name (car ps)))
      (#,compiletime-id #,stx #,@ps)))

;; An operator definition transformer involves a phase-0 binding for
;; the definition form, and a phase-1 binding for the transformer for
;; the compile-time right-hand side
(define-syntax (define-operator-definition-transformer stx)
  (syntax-parse stx
    #:literals (syntax quote)
    [(_ id
        'protocol
        in-space-expr
        #'make-prefix-id
        #'make-infix-id
        #'prefix+infix-id)
     #`(begin
         (define-syntax id
           (make-operator-definition-transformer-runtime 'protocol
                                                         in-space-expr
                                                         #'compiletime-id))
         (begin-for-syntax
           (define-syntax compiletime-id
             (make-operator-definition-transformer-compiletime #'make-prefix-id
                                                               #'make-infix-id
                                                               #'prefix+infix-id))))]))

(define-for-syntax (make-operator-definition-transformer-runtime protocol
                                                                 in-space
                                                                 compiletime-id)
  (definition-transformer
    (lambda (stx)
      (define (template->macro protocol) (if (eq? protocol 'template) 'macro protocol))
      (syntax-parse stx
        #:datum-literals (parens group block alts op)
        [(form-id ((~and alts-tag alts) (block (group q::operator-syntax-quote
                                                      (~and rhs (block body ...))))
                                        ...+))
         (define kind (template->macro protocol))
         (list (parse-operator-definitions kind
                                           stx
                                           (syntax->list #'(q.g ...))
                                           (syntax->list #'(rhs ...))
                                           in-space
                                           compiletime-id))]
        [(form-id q::operator-syntax-quote
                  (~and rhs (block body ...)))
         (list (parse-operator-definition (template->macro protocol)
                                          #'q.g
                                          #'rhs
                                          in-space
                                          compiletime-id))]))))

(begin-for-syntax
  (define-for-syntax (make-operator-definition-transformer-compiletime make-prefix-id
                                                                       make-infix-id
                                                                       prefix+infix-id)
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts op)
        [(form-id pre-parsed)
         (parse-operator-definition-rhs #'pre-parsed
                                        make-prefix-id
                                        make-infix-id)]
        [(form-id orig-stx pre-parsed ...)
         (parse-operator-definitions-rhs #'orig-stx (syntax->list #'(pre-parsed ...))
                                         make-prefix-id
                                         make-infix-id
                                         prefix+infix-id)]))))

;; ----------------------------------------

(begin-for-syntax
  (define-syntax-class :identifier-syntax-quote
    #:datum-literals (op quotes)
    (pattern (quotes g::identifier-definition-group)))

  (define-syntax-class :identifier-definition-group
    #:datum-literals (group)
    (pattern (group _:identifier . _)))
  
  (define-splicing-syntax-class :identifier-sequence-syntax-quote
    #:datum-literals (op block quotes group)
    (pattern (quotes g::identifier-definition-group
                     . gs))))

(define-for-syntax (parse-transformer-definition g rhs)
  (syntax-parse g
    #:datum-literals (group)
    [(group id:identifier . tail-pattern)
     #`(pre-parsed id
                   tail-pattern
                   #,rhs)]))

(define-syntax (define-identifier-syntax-definition-transformer stx)
  (syntax-parse stx
    #:literals (syntax)
    [(_ id in-space-expr
        #'make-transformer-id)
     #`(begin
         (define-syntax id (make-identifier-syntax-definition-transformer-runtime in-space-expr
                                                                                  #'compiletime-id))
         (begin-for-syntax
           (define-syntax compiletime-id
             (make-identifier-syntax-definition-transformer-compiletime #'make-transformer-id))))]))

(define-for-syntax (make-identifier-syntax-definition-transformer-runtime in-space
                                                                          compiletime-id)
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts op)
        [(form-id q::identifier-syntax-quote
                  (~and rhs (block
                             (~optional (group #:op_stx (block (group self-id:identifier)))
                                        #:defaults ([self-id #'self]))
                             body ...)))
         (define p (parse-transformer-definition #'q.g #'rhs))
         (list #`(define-syntax #,(in-space (pre-parsed-name p))
                   (#,compiletime-id #,p)))]))))

(begin-for-syntax
  (define-for-syntax (make-identifier-syntax-definition-transformer-compiletime make-transformer-id)
    (lambda (stx)
      (syntax-parse stx
        [(_ pre-parsed)
         (parse-transformer-definition-rhs #'pre-parsed #'self-id
                                           make-transformer-id)]))))

(define-syntax (define-identifier-syntax-definition-sequence-transformer stx)
  (syntax-parse stx
    #:literals (syntax)
    [(_ id in-space-expr
        #'make-transformer-id)
     #`(begin
         (define-syntax id (make-identifier-syntax-definition-sequence-transformer-runtime in-space-expr
                                                                                           #'compiletime-id))
         (begin-for-syntax
           (define-syntax compiletime-id
             (make-identifier-syntax-definition-sequence-transformer-compiletime #'make-transformer-id))))]))

(define-for-syntax (make-identifier-syntax-definition-sequence-transformer-runtime in-space
                                                                                   compiletime-id)
  (definition-transformer
    (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block alts op)
       [(form-id q::identifier-sequence-syntax-quote
                 (~and rhs (block
                            (~optional (group #:op_stx (block (group self-id:identifier)))
                                       #:defaults ([self-id #'self]))
                            body ...)))
        (define p (parse-transformer-definition #'q.g #'rhs))
        (list #`(define-syntax #,(in-space (pre-parsed-name p))
                  (#,compiletime-id #,p q.gs)))]))))

(begin-for-syntax
  (define-for-syntax (make-identifier-syntax-definition-sequence-transformer-compiletime make-transformer-id)
    (lambda (stx)
      (syntax-parse stx
        [(_ pre-parsed gs)
         (parse-transformer-definition-sequence-rhs #'pre-parsed #'self-id
                                                    make-transformer-id
                                                    #'gs)]))))
