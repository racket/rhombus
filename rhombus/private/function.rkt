#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "consistent.rkt")
         "expression.rkt"
         "binding.rkt"
         "definition.rkt"
         "expression+definition.rkt"
         "parse.rkt"
         "nested-bindings.rkt"
         (only-in "assign.rkt"
                  [= rhombus=]))

(provide function
         :>
         (for-syntax :kw-opt-binding
                     build-function
                     build-case-function))

(module+ for-call
  (provide (for-syntax parse-function-call)))

(begin-for-syntax
  (define-syntax-class :kw-opt-binding
    #:datum-literals (op)
    #:literals (:> rhombus=)
    (pattern (group kw:identifier (op :>) a ... (op rhombus=) e ...)
             #:with arg::binding #'(group a ...)
             #:with default #'(group e ...)
             #:attr parsed #'arg.parsed)
    (pattern (group kw:identifier (op :>) a ...)
             #:with arg::binding #'(group a ...)
             #:attr default #'#f
             #:attr parsed #'arg.parsed)
    (pattern arg::binding
             #:attr default #'#f
             #:attr kw #'#f
             #:attr parsed #'arg.parsed)))

(define-syntax function
  (make-expression+definition-transformer
   (expression-transformer
    #'function
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts)
        [(form-id ((~and alts-tag alts) (block (group (parens arg::binding ...) (~and rhs (block body ...)))) ...+) . tail)
         (values
          (build-case-function #'form-id #'((arg ...) ...) #'((arg.parsed ...) ...) #'(rhs ...) #'form-id #'alts-tag)
          #'tail)]
        [(form-id ((~and parens-tag parens) arg::kw-opt-binding ...) (~and rhs (block body ...)) . tail)
         (values
          (build-function #'form-id #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...) #'rhs #'form-id #'parens-tag)
          #'tail)])))
   (definition-transformer
     (lambda (stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts)
        [(form-id ((~and alts-tag alts) (block (group name:identifier (parens arg::binding ...) (~and rhs (block body ...)))) ...+))
         (define names (syntax->list #'(name ...)))
         (define the-name (car names))
         (check-consistent stx names "name")
         (list
          #`(define #,the-name
              #,(build-case-function #'form-id #'((arg ...) ...) #'((arg.parsed ...) ...) #'(rhs ...) #'form-id #'alts-tag)))]
        [(form-id name:identifier ((~and parens-tag parens) arg::kw-opt-binding ...) (~and rhs (block body ...)))
         (list
          #`(define name
              #,(build-function #'form-id #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...) #'rhs #'form-id #'parens-tag)))]
        ;; definition form didn't match, so try parsing as a `function` expression:
        [(_ ((~and parens-tag parens) arg ...) (~and rhs (block . _)) . _)
         (syntax-parse #`(group . #,stx)
           [e::expression
            (list #'e.parsed)])])))))

(begin-for-syntax

  (struct fcase (args arg-parseds rhs))
  
  (define (group-by-counts fcases)
    (define ht
      (for/fold ([ht #hasheqv()]) ([fc (in-list fcases)])
        (define n (length (fcase-args fc)))
        (hash-set ht n (cons fc (hash-ref ht n '())))))
    (for/list ([sames (in-hash-values ht)])
      (reverse sames)))

  (define (build-function function-name kws args arg-parseds defaults rhs start end)
    (define arg-ids (generate-temporaries args))
    (with-syntax ([(arg-id ...) arg-ids]
                  [(arg ...) args]
                  [(arg.parsed ...) arg-parseds]
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
              #f argument-binding-failure
              (begin)
              (arg-id arg.parsed arg)
              ...
              (rhombus-expression (group rhs))))))))
  
  (define (build-case-function function-name argss-stx arg-parsedss-stx rhss-stx start end)
    (define argss (map syntax->list (syntax->list argss-stx)))
    (define arg-parsedss (map syntax->list (syntax->list arg-parsedss-stx)))
    (define rhss (syntax->list rhss-stx))
    (define sames (group-by-counts (map fcase argss arg-parsedss rhss)))
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
                                        [(arg-parsed ...) (fcase-arg-parseds (car same))]
                                        [rhs (fcase-rhs (car same))])
                            #`(let ([try-next (lambda () #,(loop (cdr same)))])
                                (nested-bindings
                                 #,function-name
                                 try-next argument-binding-failure
                                 (begin)
                                 (arg-id arg-parsed arg)
                                 ...
                                 (rhombus-expression (group rhs)))))]))]))))))

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
             #:attr parsed #'exp.parsed)
    (pattern exp::expression
             #:attr kw #'#f
             #:attr parsed #'exp.parsed)))

(define-for-syntax (parse-function-call rator stxes)
  (syntax-parse stxes
    [(_ ((~and head (~datum parens)) rand::kw-expression ...) . tail)
     #:with ((arg-form ...) ...) (for/list ([kw (in-list (syntax->list #'(rand.kw ...)))]
                                            [parsed (in-list (syntax->list #'(rand.parsed ...)))])
                                   (if (syntax-e kw)
                                       (list (string->keyword (symbol->string (syntax-e kw))) parsed)
                                       (list parsed)))
     (values (datum->syntax (quote-syntax here)
                            (cons rator #'(arg-form ... ...))
                            (span-srcloc rator #'head)
                            #'head)
             #'tail)]))
