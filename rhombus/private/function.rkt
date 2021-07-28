#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "transformer.rkt"
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         (only-in "assign.rkt"
                  [= rhombus=]))

(provide function
         :>
         (for-syntax :kw-opt-binding
                     build-function
                     build-case-function))

(module+ for-call
  (provide (for-syntax parse-function-call)
           nested-bindings))

(begin-for-syntax
  (define-syntax-class :kw-opt-binding
    #:datum-literals (op)
    #:literals (:> rhombus=)
    (pattern (group kw:identifier (op :>) a ... (op rhombus=) e ...)
             #:with arg::binding #'(group a ...)
             #:with default #'(group e ...)
             #:attr expanded #'arg.expanded)
    (pattern (group kw:identifier (op :>) a ...)
             #:with arg::binding #'(group a ...)
             #:attr default #'#f
             #:attr expanded #'arg.expanded)
    (pattern arg::binding
             #:attr default #'#f
             #:attr kw #'#f
             #:attr expanded #'arg.expanded)))

(define-syntax function
  (expression-transformer
   #'function
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block alts)
       [(form-id:identifier ((~and alts-tag alts) (block (group (parens arg::binding ...) (~and rhs (block body ...)))) ...+) . tail)
        (define argss (map syntax->list (syntax->list #'((arg ...) ...))))
        (define arg-expandedss (map syntax->list (syntax->list #'((arg.expanded ...) ...))))
        (define rhss (syntax->list #'(rhs ...)))
        (values
         (build-case-function #'form-id argss arg-expandedss rhss #'form-id #'alts-tag)
         #'tail)]
       [(form-id:identifier ((~and parens-tag parens) arg::kw-opt-binding ...) (~and rhs (block body ...)) . tail)
        (values
         (build-function #'form-id #'(arg.kw ...) #'(arg ...) #'(arg.expanded ...) #'(arg.default ...) #'rhs #'form-id #'parens-tag)
         #'tail)]))))

(begin-for-syntax

  (struct fcase (args arg-expandeds rhs))
  
  (define (group-by-counts fcases)
    (define ht
      (for/fold ([ht #hasheqv()]) ([fc (in-list fcases)])
        (define n (length (fcase-args fc)))
        (hash-set ht n (cons fc (hash-ref ht n '())))))
    (for/list ([sames (in-hash-values ht)])
      (reverse sames)))

  (define (build-function function-name kws args arg-expandeds defaults rhs start end)
    (define arg-ids (generate-temporaries args))
    (with-syntax ([(arg-id ...) arg-ids]
                  [(arg ...) args]
                  [(arg.expanded ...) arg-expandeds]
                  [rhs rhs])
      (with-syntax ([((arg-form ...) ...)
                     (for/list ([kw (in-list (syntax->list kws))]
                                [arg-id (in-list arg-ids)]
                                [default (in-list (syntax->list defaults))])
                       (define arg+default
                         (cond
                           [(not (syntax-e default))
                            arg-id]
                           [else
                            #`[#,arg-id (rhombus-expression #,default)]]))
                       (cond
                         [(not (syntax-e kw))
                          (list arg+default)]
                         [else
                          (list (string->keyword (symbol->string (syntax-e kw))) arg+default)]))])
        (relocate
         (span-srcloc start end)
         #`(lambda (arg-form ... ...)
             (nested-bindings
              #,function-name
              #f
              (begin)
              (arg-id arg.expanded arg)
              ...
              (rhombus-expression (group rhs))))))))
  
  (define (build-case-function function-name argss arg-expandedss rhss start end)
    (define sames (group-by-counts (map fcase argss arg-expandedss rhss)))
    (relocate
     (span-srcloc start end)
     #`(case-lambda
         #,@(for/list ([same (in-list sames)])
              (with-syntax ([(try-next arg-id ...) (generate-temporaries
                                                    (cons 'try-next (fcase-args (car same))))])
                #`[(arg-id ...)
                   #,(let loop ([same same])
                       (cond
                         [(null? same)
                          #`(cases-failure '#,function-name arg-id ...)]
                         [else
                          (with-syntax ([(arg ...) (fcase-args (car same))]
                                        [(arg-expanded ...) (fcase-arg-expandeds (car same))]
                                        [rhs (fcase-rhs (car same))])
                            #`(let ([try-next (lambda () #,(loop (cdr same)))])
                                (nested-bindings
                                 #,function-name
                                 try-next
                                 (begin)
                                 (arg-id arg-expanded arg)
                                 ...
                                 (rhombus-expression (group rhs)))))]))]))))))

(define-syntax (nested-bindings stx)
  (syntax-parse stx
    [(_ who try-next post-defn body) #'(let () post-defn body)]
    [(_ who try-next post-defn (arg-id arg::binding-form arg-pat) . tail)
     #'(let-values ([(match? . arg.var-ids) (arg.check-proc-expr arg-id)])
         (if match?
             (nested-bindings
              who
              try-next
              (begin post-defn arg.post-defn)
              . tail)
             (if try-next
                 (try-next)
                 (argument-binding-failure 'who arg-id 'arg-pat))))]))

(define (argument-binding-failure who val binding)
  (raise-binding-failure who "argument" val binding))

(define (cases-failure who . args)
  (apply error who
         (apply string-append "no matching case for arguments\n"
                "  arguments...:"
                (for/list ([arg (in-list args)])
                  "\n   ~e"))
         args))

(define-syntax :> #f)

(begin-for-syntax
  (define-syntax-class :kw-expression
    #:datum-literals (op)
    #:literals (:>)
    (pattern (group kw:identifier (op :>) e ...)
             #:with exp::expression #'(group e ...)
             #:attr expanded #'exp.expanded)
    (pattern exp::expression
             #:attr kw #'#f
             #:attr expanded #'exp.expanded)))

(define-for-syntax (parse-function-call rator stxes)
  (syntax-parse stxes
    [(((~and head (~datum parens)) rand::kw-expression ...) . tail)
     #:with ((arg-form ...) ...) (for/list ([kw (in-list (syntax->list #'(rand.kw ...)))]
                                            [expanded (in-list (syntax->list #'(rand.expanded ...)))])
                                   (if (syntax-e kw)
                                       (list (string->keyword (symbol->string (syntax-e kw))) expanded)
                                       (list expanded)))
     (values (datum->syntax (quote-syntax here)
                            (cons rator #'(arg-form ... ...))
                            (span-srcloc rator #'head)
                            #'head)
             #'tail)]))
