#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     "tag.rkt")
         "syntax-class-clause.rkt"
         (submod "syntax-class-clause.rkt" for-class)
         "parens.rkt"
         "parse.rkt"
         "op-literal.rkt"
         "pack.rkt")

(provide (for-space rhombus/syntax_class_clause
                    description
                    kind
                    error_mode
                    fields
                    root_swap))

(module+ for-class
  (begin-for-syntax
    (provide extract-clauses
             (struct-out declared-field))))

(define-for-syntax (extract-clauses stx clauses orig-pats)
  (define-values (options spliced-pats)
    (let loop ([orig-stx #f]
               [options #hasheq()]
               [spliced-pats #f]
               [clauses clauses])
      (for/fold ([options options]
                 [spliced-pats spliced-pats])
                ([clause (in-list clauses)])
        (define (check what)
          (syntax-parse clause
            [(kw . _)
             (when (hash-ref options (syntax-e #'kw) #f)
               (raise-syntax-error #f
                                   (string-append "found second " what " clause, but only one is allowed")
                                   stx
                                   (or orig-stx
                                       (syntax-parse clause
                                         [(_ orig-stx . _) #'orig-stx]))))]))
        (define (check-splice orig-stx)
          (when orig-pats
            (raise-syntax-error #f "patterns already supplied" stx orig-stx))
          (when spliced-pats
            (raise-syntax-error #f "duplicate spliced patterns" stx orig-stx)))
        (syntax-parse clause
          [(#:description _ e)
           (check "description")
           (values (hash-set options '#:description #'e)
                   spliced-pats)]
          [(#:fields _ ht)
           (check "fields")
           (values (hash-set options '#:fields (syntax-e #'ht))
                   spliced-pats)]
          [(#:root_swap _ to-root root-to)
           (check "root_swap")
           (values (hash-set options '#:root_swap (cons #'to-root #'root-to))
                   spliced-pats)]
          [(#:kind _ kw)
           (check "kind")
           (values (hash-set options '#:kind (syntax-e #'kw))
                   spliced-pats)]
          [(#:error-mode _ kw)
           (check "error mode")
           (values (hash-set options '#:error-mode (syntax-e #'kw))
                   spliced-pats)]
          [(#:splice orig-stx (~and cls (_ ...)))
           (loop #'orig-stx options spliced-pats (syntax->list #'cls))]
          [(#:splice/alts orig-stx (~and alts (_ ...)))
           (check-splice #'orig-stx)
           (values options (syntax->list #'alts))]
          [(#:splice/alts orig-stx (~and cls (_ ...)) (~and alts (_ ...)))
           (check-splice #'orig-stx)
           (loop #'orig-stx options (syntax->list #'alts) (syntax->list #'cls))]))))
  (define pats (or orig-pats spliced-pats))
  (unless pats
    (raise-syntax-error #f "no patterns supplied" stx))
  (values pats
          (hash-ref options '#:kind #f)
          (hash-ref options '#:description #f)
          (hash-ref options '#:fields #f)
          (hash-ref options '#:root_swap #f)
          (eq? (hash-ref options '#:error-mode #f) '#:opaque)))

(define-syntax-class-clause-syntax description
  (syntax-class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (tag::block e ...))
        #`(#:description #,stx (rhombus-body-at tag e ...))]
       [(_ e ...)
        #`(#:description #,stx (rhombus-expression (#,group-tag e ...)))]))))

(begin-for-syntax
  (struct declared-field (id depth unpack*-id) #:prefab)
  (define-syntax-class :kind-id
    #:attributes (name)
    #:description "the literal `kind`"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? (in-syntax-class-clause-space #'name)
                                       (syntax-class-clause-quote kind)))))

(define-syntax-class-clause-syntax fields
  (syntax-class-clause-transformer
   (lambda (stx)
     (define (parse-fields field-lines-stx)
       (define ht (for/fold ([ht #hasheq()]) ([field-line-stx (in-list (syntax->list field-lines-stx))])
                    (let loop ([field-line-stx field-line-stx] [ht ht])
                      (define (one id depth)
                        (define sym (syntax-e id))
                        (when (hash-ref ht sym #f)
                          (raise-syntax-error #f
                                              "duplicate field name"
                                              stx
                                              id))
                        (define-values (kind rest)
                          (syntax-parse field-line-stx
                            #:datum-literals (group)
                            [(_ (_::block
                                 ~!
                                 (group _::kind-id
                                        (~or*
                                         (_::block
                                          (group (~and kind (~or* #:term #:sequence #:group #:multi #:block))))
                                         (~and kind (~or* #:term #:sequence #:group #:multi #:block))))))
                             (values #'kind #'())]
                            [(_ . rest)
                             (values #'#f #'rest)]))
                        (define unpack*-id
                          (case (syntax-e kind)
                            [(#:term) #'unpack-term*]
                            [(#:sequence) #'unpack-term-list*]
                            [(#:group) #'unpack-group*]
                            [(#:multi) #'unpack-multi*]
                            [else #'#f]))
                        (loop rest (hash-set ht sym (declared-field id depth unpack*-id))))
                      (syntax-parse field-line-stx
                        #:datum-literals (group)
                        [() ht]
                        [(id:identifier . _)
                         (one #'id 0)]
                        [((_::brackets g (group _::...-bind)) . _)
                         (let loop ([g #'g] [depth 1])
                           (syntax-parse g
                             #:datum-literals (group)
                             [(group id:identifier) (one #'id depth)]
                             [(group (_::brackets g (group _::...-bind)))
                              (loop #'d (add1 depth))]))]))))
       #`(#:fields #,stx #,ht))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::block (group fld ...) ...))
        (parse-fields #'((fld ...) ...))]
       [(_ fld ...)
        (parse-fields #'((fld ...)))]))))

(define-syntax-class-clause-syntax root_swap
  (syntax-class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (tag::block (group to-root:identifier root-to:identifier)))
        #`(#:root_swap #,stx to-root root-to)]))))

(define-for-syntax (make-kw-clause tag-kw valid what)
  (syntax-class-clause-transformer
   (lambda (stx)
     (define (parse-keyword kw)
       (if (memq (syntax-e kw) valid)
           kw
           (raise-syntax-error #f
                               (string-append "not a recognized " what)
                               stx
                               kw)))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ kw:keyword) #`(#,tag-kw #,stx #,(parse-keyword #'kw))]
       [(_ (_::block (group kw:keyword))) #`(#,tag-kw #,stx #,(parse-keyword #'kw))]))))

(define-syntax-class-clause-syntax kind (make-kw-clause '#:kind '(#:term #:sequence #:group #:multi #:block) "kind"))
(define-syntax-class-clause-syntax error_mode (make-kw-clause '#:error-mode '(#:opaque #:transparent) "error mode"))
