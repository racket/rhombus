#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "transformer.rkt")
         "binding.rkt"
         "parse.rkt")

(provide (rename-out [rhombus-define define]))

(begin-for-syntax
  (struct fcase (args arg-expandeds rhs))
  (define (group-by-counts fcases)
    (define ht
      (for/fold ([ht #hasheqv()]) ([fc (in-list fcases)])
        (define n (length (fcase-args fc)))
        (hash-set ht n (cons fc (hash-ref ht n '())))))
    (for/list ([sames (in-hash-values ht)])
      (reverse sames))))

(define-syntax rhombus-define
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block alts)
       [(form-id:identifier (alts (block (group id:identifier (parens arg::binding ...) (~and rhs (block body ...)))) ...+))
        (define ids (syntax->list #'(id ...)))
        (define the-id (car ids))
        (for ([another-id (in-list (cdr ids))])
          (unless (free-identifier=? the-id another-id)
            (raise-syntax-error #f
                                "case name does not match initial case name"
                                stx
                                another-id)))
        (define argss (map syntax->list (syntax->list #'((arg ...) ...))))
        (define arg-expandedss (map syntax->list (syntax->list #'((arg.expanded ...) ...))))
        (define rhss (syntax->list #'(rhs ...)))
        (define sames (group-by-counts (map fcase argss arg-expandedss rhss)))
        (values
         (list
          #`(define #,the-id
              (case-lambda
                #,@(for/list ([same (in-list sames)])
                     (with-syntax ([(try-next arg-id ...) (generate-temporaries
                                                           (cons 'try-next (fcase-args (car same))))])
                       #`[(arg-id ...)
                          #,(let loop ([same same])
                              (cond
                                [(null? same)
                                 #`(cases-failure '#,the-id arg-id ...)]
                                [else
                                 (with-syntax ([(arg ...) (fcase-args (car same))]
                                               [(arg-expanded ...) (fcase-arg-expandeds (car same))]
                                               [rhs (fcase-rhs (car same))])
                                   #`(let ([try-next (lambda () #,(loop (cdr same)))])
                                       (nested-bindings
                                        form-id
                                        try-next
                                        (begin)
                                        (arg-id arg-expanded arg)
                                        ...
                                        (rhombus-expression (group rhs)))))]))])))))
         null)]
       [(form-id:identifier id::non-binding-identifier (parens arg::binding ...) (~and rhs (block body ...)))
        #:with (arg-id ...) (generate-temporaries #'(arg ...))
        (values
         (list
          #'(define id
              (lambda (arg-id ...)
                (nested-bindings
                 form-id
                 #f
                 (begin)
                 (arg-id arg.expanded arg)
                 ...
                 (rhombus-expression (group rhs))))))
         null)]
       [(form-id:identifier any ... (~and rhs (block body ...)))
        #:with lhs::binding #'(group any ...)
        #:with lhs-e::binding-form #'lhs.expanded
        #:with name-id (syntax-parse #'lhs-e.var-ids
                         [(id) #'id]
                         [_ (quote-syntax rhs)])
        (values
         (list
          #'(define-values lhs-e.var-ids
              (let ([tmp-id (let ([name-id (rhombus-expression (group rhs))])
                              name-id)])
                (let-values ([(match? . lhs-e.var-ids)
                              (lhs-e.check-proc-expr tmp-id)])
                  (unless match?
                    (rhs-binding-failure 'form-id tmp-id 'lhs))
                  (values . lhs-e.var-ids))))
          #'lhs-e.post-defn)
         null)]))))

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
  (binding-failure who "argument" val binding))

(define (rhs-binding-failure who val binding)
  (binding-failure who "value" val binding))

(define (binding-failure who what val binding)
  (error who
         (string-append "~a does not match binding pattern\n"
                        "  argument: ~v\n"
                        "  binding: ~s")
         what
         val
         binding))

(define (cases-failure who . args)
  (apply error who
         (apply string-append "no matching case for arguments\n"
                "  arguments...:"
                (for/list ([arg (in-list args)])
                  "\n   ~e"))
         args))
