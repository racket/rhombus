#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol
                     "tag.rkt")
         "space-meta-clause.rkt"
         (submod "space-meta-clause.rkt" for-class)
         "parens.rkt"
         "parse.rkt")

(provide (for-space rhombus/space_meta_clause
                    parse_syntax_class
                    parse_prefix_more_syntax_class
                    parse_infix_more_syntax_class
                    name_start_syntax_class
                    reflection
                    description
                    operator_description
                    parse_checker
                    parsed_packer
                    parsed_unpacker
                    identifier_parser
                    private))

(module+ for-space-meta-macro
  (provide rhombus-space-meta-clause
           (for-syntax parse-space-meta-clause-options)))

(define-syntax rhombus-space-meta-clause 'placeholder)

(define-for-syntax (wrap-clause parsed)
  #`[(group (parsed #:rhombus/space_meta_clause (quote-syntax (rhombus-space-meta-clause #,parsed) #:local)))])
(define-for-syntax (unwrap-clause parsed)
  (syntax-parse parsed
    #:datum-literals (group parsed quote-syntax rhombus-space-meta-clause)
    [[(group (parsed #:rhombus/space_meta_clause (quote-syntax (rhombus-space-meta-clause p) #:local)))]
     #'p]))

(define-for-syntax (make-identifier-transformer kw)
  (space-meta-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier)
        (wrap-clause #`(#,kw id #t))]))))

(define-space-meta-clause-syntax parse_syntax_class
  (space-meta-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ id:identifier)
        (wrap-clause #`(#:syntax_class id #t))]
       [(_ id:identifier (_::parens (group arg:id) ...))
        (wrap-clause #`(#:syntax_class id #t (arg ...)))]))))
(define-space-meta-clause-syntax parse_prefix_more_syntax_class
  (make-identifier-transformer '#:syntax_class_prefix_more))
(define-space-meta-clause-syntax parse_infix_more_syntax_class
  (make-identifier-transformer '#:syntax_class_infix_more))
(define-space-meta-clause-syntax name_start_syntax_class
  (make-identifier-transformer '#:syntax_class_name_start))
(define-space-meta-clause-syntax reflection
  (make-identifier-transformer '#:reflection))
(define-space-meta-clause-syntax parsed_packer
  (make-identifier-transformer '#:parsed_packer))
(define-space-meta-clause-syntax parsed_unpacker
  (make-identifier-transformer '#:parsed_unpacker))

(define-for-syntax (make-expression-transformer kw)
  (space-meta-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id (tag::block g ...))
        (wrap-clause #`(#,kw #,stx (rhombus-body-at tag g ...)))]
       [(form-id e ...)
        (wrap-clause #`(#,kw #,stx (rhombus-expression (#,group-tag e ...))))]))))

(define-space-meta-clause-syntax description
  (make-expression-transformer '#:desc))
(define-space-meta-clause-syntax operator_description
  (make-expression-transformer '#:operator_desc))
(define-space-meta-clause-syntax parse_checker
  (make-expression-transformer '#:parsed_checker))
(define-space-meta-clause-syntax identifier_parser
  (make-expression-transformer '#:identifier_transformer))

(define-space-meta-clause-syntax private
  (space-meta-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier . rest)
        #:when (let ([id (in-space-meta-clause-space #'id)])
                 (or (free-identifier=? id (in-space-meta-clause-space (quote-syntax parse_syntax_class)))
                     (free-identifier=? id (in-space-meta-clause-space (quote-syntax parse_prefix_more_syntax_class)))
                     (free-identifier=? id (in-space-meta-clause-space (quote-syntax parse_infix_more_syntax_class)))
                     (free-identifier=? id (in-space-meta-clause-space (quote-syntax name_start_syntax_class)))
                     (free-identifier=? id (in-space-meta-clause-space (quote-syntax reflection)))
                     (free-identifier=? id (in-space-meta-clause-space (quote-syntax parsed_packer)))
                     (free-identifier=? id (in-space-meta-clause-space (quote-syntax parsed_unpacker)))))
        (syntax-parse #'(group id . rest)
          [cl::space-meta-clause
           (syntax-parse (unwrap-clause #'cl.parsed)
             [(kw id #t . rest)
              (wrap-clause #'(kw id #f . rest))])])]
       [else
        (raise-syntax-error #f
                            "expected enforest syntax class, reflection, packer, or unpacker clause to make private"
                            stx)]))))

(define-for-syntax (parse-space-meta-clause-options orig-stx enforest? options-stx)
  (for/fold ([options #hasheq()]) ([option (in-list (syntax->list options-stx))])
    (define (check what #:enforest-only? [enforest-only? #f])
      (syntax-parse option
        [(_ (kw stx . _))
         (unless (or enforest? (not enforest-only?))
           (raise-syntax-error #f (format "~a not allowed in a transformer" what) orig-stx #'stx))
         (when (hash-ref options (syntax-e #'kw) #f)
           (raise-syntax-error #f (format "multiple ~a declared" what) orig-stx #'stx))]))
    (define (maybe-private options public?-stx kw)
      (if (syntax-e public?-stx)
          options
          (hash-set options '#:private (hash-set (hash-ref options #'#:private #hasheq()) kw #t))))
    (syntax-parse option
      [(_ (#:syntax_class id public?))
       (check "syntax classes")
       (maybe-private (hash-set options '#:syntax_class #'id)
                      #'public?
                      '#:syntax_class)]
      [(_ (#:syntax_class id public? (arg ...)))
       (check "syntax classes")
       (maybe-private (hash-set (hash-set options '#:syntax_class #'id)
                                '#:syntax_class_arguments (syntax->list #'(arg ...)))
                      #'public?
                      '#:syntax_class)]
      [(_ (#:syntax_class_prefix_more id public?))
       (check "prefix-more syntax classes" #:enforest-only? #t)
       (maybe-private (hash-set options '#:syntax_class_prefix_more #'id)
                      #'public?
                      '#:syntax_class_prefix_more)]
      [(_ (#:syntax_class_infix_more id public?))
       (check "infix-more syntax classes" #:enforest-only? #t)
       (maybe-private (hash-set options '#:syntax_class_infix_more #'id)
                      #'public?
                      '#:syntax_class_infix_more)]
      [(_ (#:syntax_class_name_start id public?))
       (check "name-start syntax classes")
       (maybe-private (hash-set options '#:syntax_class_name_start #'id)
                      #'public?
                      '#:syntax_class_name_start)]
      [(_ (#:reflection id public?))
       (check "syntax_value names")
       (maybe-private (hash-set options '#:reflection #'id)
                      #'public?
                      '#:reflection)]
      [(_ (#:desc stx e))
       (check "description string expressions")
       (hash-set options '#:desc #'e)]
      [(_ (#:operator_desc stx e))
       (check "operator description string expressions" #:enforest-only? #t)
       (hash-set options '#:operator_desc #'e)]
      [(_ (#:parsed_checker stx e))
       (check "parse-checking function expressions")
       (hash-set options '#:parsed_checker #'e)]
      [(_ (#:parsed_packer pack public?))
       (when (hash-ref options '#:parsed_packer #f)
         (raise-syntax-error #f "multiple parsed packer names declared" orig-stx #'pack))
       (maybe-private (hash-set options '#:parsed_packer #'pack)
                      #'public?
                      '#:parsed_packer)]
      [(_ (#:parsed_unpacker unpack public?))
       (when (hash-ref options '#:parsed_unpacker #f)
         (raise-syntax-error #f "multiple parsed unpacker names declared" orig-stx #'unpack))
       (maybe-private (hash-set options '#:parsed_unpacker #'unpack)
                      #'public?
                      '#:parsed_unpacker)]
      [(_ (#:identifier_transformer stx e))
       (check "identifier parser expressions" #:enforest-only? #t)
       (hash-set options '#:identifier_transformer #'e)]
      [else
       (error "unhandled" option)])))
