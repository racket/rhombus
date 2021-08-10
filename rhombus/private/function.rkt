#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "consistent.rkt"
                     "with-syntax.rkt")
         racket/unsafe/undefined
         "expression.rkt"
         "binding.rkt"
         "definition.rkt"
         "expression+definition.rkt"
         "parse.rkt"
         "nested-bindings.rkt"
         "call-result-key.rkt"
         "ref-result-key.rkt"
         "static-info.rkt"
         "contract.rkt"
         (only-in "quasiquote.rkt"
                  [... rhombus...])
         (submod "contract.rkt" for-struct)
         (only-in "assign.rkt"
                  [= rhombus=]))

(provide fun)

(module+ for-build
  (provide (for-syntax :kw-opt-binding
                       :ret-contract
                       :maybe-arg-rest
                       :non-...-binding
                       build-function
                       build-case-function)))

(module+ for-call
  (provide (for-syntax parse-function-call)))

(begin-for-syntax
  (define-syntax-class :non-...-binding
    (pattern form
             #:when (syntax-parse #'form
                      #:datum-literals (group op)
                      [(group (op (~literal rhombus...))) #f]
                      [_ #t])
             #:with arg::binding #'form
             #:with parsed #'arg.parsed))
  
  (define (empty->keyword g kw)
    (syntax-parse g
      [(_) #`(group #,(datum->syntax kw (string->symbol (keyword->string (syntax-e kw))) kw))]
      [_ g]))
  
  (define-syntax-class :kw-opt-binding
    #:datum-literals (op block group)
    #:literals (rhombus= rhombus...)
    (pattern (group kw:keyword (block (group a ... (op rhombus=) e ...+)))
             #:with arg::binding (empty->keyword #'(group a ...) #'kw)
             #:with default #'(group e ...)
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword (block (group (op rhombus=) e ...+)))
             #:with default #'(group e ...)
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword (op rhombus=) e ...+)
             #:with arg::binding (empty->keyword #'(group) #'kw)
             #:with default #'(group e ...)
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword (block (group a ...)))
             #:with arg::binding (empty->keyword #'(group a ...) #'kw)
             #:attr default #'#f
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword)
             #:with arg::binding (empty->keyword #'(group) #'kw)
             #:attr default #'#f
             #:attr parsed #'arg.parsed)
    (pattern (group a ...+ (op rhombus=) e ...+)
             #:with arg::binding #'(group a ...)
             #:with default #'(group e ...)
             #:attr kw #'#f
             #:attr parsed #'arg.parsed)
    (pattern arg::non-...-binding
             #:attr default #'#f
             #:attr kw #'#f
             #:attr parsed #'arg.parsed))

  (define-syntax-class :not-block
    #:datum-literals (op parens braces)
    (pattern _:identifier)
    (pattern (op . _))
    (pattern (parens . _))
    (pattern (braces . _)))

  (define-splicing-syntax-class :ret-contract
    #:datum-literals (block group op)
    #:literals (::)
    (pattern (~seq (op ::) ctc::not-block ...)
             #:with c::contract #'(group ctc ...)
             #:with c-parsed::contract-form #'c.parsed
             #:attr static-infos #'c-parsed.static-infos
             #:attr predicate #'c-parsed.predicate)
    (pattern (~seq)
             #:attr static-infos #'()
             #:attr predicate #'#f))

  (define-splicing-syntax-class :maybe-arg-rest
    #:datum-literals (group op)
    #:literals (rhombus...)
    (pattern (~seq arg::non-...-binding (group (op rhombus...)))
             #:with parsed #'arg.parsed)
    (pattern (~seq)
             #:attr arg #'#f
             #:attr parsed #'#f)))

(define-syntax fun
  (make-expression+definition-transformer
   (expression-transformer
    #'fun
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts)
        [(form-id ((~and alts-tag alts)
                   (block (group (parens arg::non-...-binding ... rest::maybe-arg-rest) ret::ret-contract
                                 (~and rhs (block body ...))))
                   ...+)
                  . tail)
         (values
          (build-case-function #'form-id
                               #'((arg ...) ...) #'((arg.parsed ...) ...)
                               #'(rest.arg ...) #'(rest.parsed ...)
                               #'(ret.predicate ...)
                               #'(rhs ...)
                               #'form-id #'alts-tag)
          #'tail)]
        [(form-id ((~and parens-tag parens) arg::kw-opt-binding ... rest::maybe-arg-rest) ret::ret-contract
                  (~and rhs (block body ...))
                  . tail)
         (values
          (build-function #'form-id
                          #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                          #'rest.arg #'rest.parsed
                          #'ret.predicate
                          #'rhs
                          #'form-id #'parens-tag)
          #'tail)])))
   (definition-transformer
     (lambda (stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts)
        [(form-id ((~and alts-tag alts)
                   (block (group name:identifier (parens arg::non-...-binding ... rest::maybe-arg-rest)
                                 ret::ret-contract
                                 (~and rhs (block body ...))))
                   ...+))
         (define names (syntax->list #'(name ...)))
         (define the-name (car names))
         (check-consistent stx names "name")
         (maybe-add-function-result-definition
          the-name (syntax->list #'(ret.static-infos ...))
          (list
           #`(define #,the-name
               #,(build-case-function #'form-id
                                      #'((arg ...) ...) #'((arg.parsed ...) ...)
                                      #'(rest.arg ...) #'(rest.parsed ...)
                                      #'(ret.predicate ...)
                                      #'(rhs ...)
                                      #'form-id #'alts-tag))))]
        [(form-id name:identifier ((~and parens-tag parens) arg::kw-opt-binding ... rest::maybe-arg-rest)
                  ret::ret-contract
                  (~and rhs (block body ...)))
         (maybe-add-function-result-definition
          #'name (list #'ret.static-infos)
          (list
           #`(define name
               #,(build-function #'form-id
                                 #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                                 #'rest.arg #'rest.parsed
                                 #'ret.predicate
                                 #'rhs
                                 #'form-id #'parens-tag))))]
        ;; definition form didn't match, so try parsing as a `fun` expression:
        [(_ (~or (parens _ ...)
                 (alts (block (group (parens _ ...))) ...+))
            . _)
         (syntax-parse #`(group . #,stx)
           [e::expression
            (list #'e.parsed)])])))))

(begin-for-syntax

  (struct fcase (args arg-parseds rest-arg rest-arg-parsed pred rhs))

  (define (build-function function-name
                          kws args arg-parseds defaults
                          rest-arg rest-parsed
                          pred
                          rhs
                          start end)
    (with-syntax-parse ([(arg-parsed::binding-form ...) arg-parseds])
      (with-syntax ([(tmp-id ...) (generate-temporaries #'(arg-parsed.arg-id ...))]
                    [(arg ...) args]
                    [rhs rhs]
                    [(maybe-rest-tmp maybe-match-rest
                                     (maybe-bind-rest ...)
                                     (maybe-bind-rest-static-rest ...))
                     (if (syntax-e rest-arg)
                         (with-syntax-parse ([rest::binding-form rest-parsed])
                           #`(rest-tmp
                              (rest-tmp rest-getter #,rest-parsed #,rest-arg)
                              ((define-values (rest.bind-id ...) (rest-getter)))
                              ((define-static-info-syntax/maybe rest.bind-id (#%ref-result (rest.bind-static-info ...)))
                               ...)))
                         #'(() #f () ()))])
        (with-syntax ([(((arg-form ...) arg-default) ...)
                       (for/list ([kw (in-list (syntax->list kws))]
                                  [tmp-id (in-list (syntax->list #'(tmp-id ...)))]
                                  [default (in-list (syntax->list defaults))])
                         ;; FIXME: if `default` is simple enough, then
                         ;; use it instead of `unsafe-undefined`, and
                         ;; then `define` has the opportunity to inline it
                         (define arg+default
                           (cond
                             [(not (syntax-e default))
                              tmp-id]
                             [else
                              #`[#,tmp-id unsafe-undefined]]))
                         (cond
                           [(not (syntax-e kw))
                            (list (list arg+default) default)]
                           [else
                            (list (list kw arg+default) default)]))])
          (relocate
           (span-srcloc start end)
           #`(lambda (arg-form ... ... . maybe-rest-tmp)
               (nested-bindings
                #,function-name
                #f argument-binding-failure
                (tmp-id arg-parsed arg arg-default)
                ...
                maybe-match-rest
                (begin
                  (arg-parsed.binder-id tmp-id arg-parsed.data) ...
                  maybe-bind-rest ...
                  (begin
                    (define-static-info-syntax/maybe arg-parsed.bind-id arg-parsed.bind-static-info ...)
                    ...)
                  ...
                  maybe-bind-rest-static-rest ...
                  (add-contract-check
                   #,function-name #,pred
                   (rhombus-expression (group rhs)))))))))))
  
  (define (build-case-function function-name
                               argss-stx arg-parsedss-stx
                               rest-args-stx rest-parseds-stx
                               preds-stx
                               rhss-stx
                               start end)
    (define argss (map syntax->list (syntax->list argss-stx)))
    (define arg-parsedss (map syntax->list (syntax->list arg-parsedss-stx)))
    (define rest-args (syntax->list rest-args-stx))
    (define rest-parseds (syntax->list rest-parseds-stx))
    (define preds (syntax->list preds-stx))
    (define rhss (syntax->list rhss-stx))
    (define n+sames (group-by-counts (map fcase argss arg-parsedss rest-args rest-parseds preds rhss)))
    (relocate
     (span-srcloc start end)
     #`(case-lambda
         #,@(for/list ([n+same (in-list n+sames)])
              (define n (car n+same))
              (define same (cdr n+same))
              (with-syntax ([(try-next arg-id ...) (generate-temporaries
                                                    (cons 'try-next
                                                          (fcase-args (find-matching-case n same))))]
                            [maybe-rest-tmp (if (negative? n)
                                                #'rest-tmp
                                                #'())]
                            [maybe-rest-tmp-use (if (negative? n)
                                                    #'rest-tmp
                                                    #'null)])
                #`[(arg-id ... . maybe-rest-tmp)
                   #,(let loop ([same same])
                       (cond
                         [(null? same)
                          #`(cases-failure '#,function-name maybe-rest-tmp-use arg-id ...)]
                         [else
                          (define fc (car same))
                          (define-values (this-args wrap-adapted-arguments)
                            (adapt-arguments-for-count fc n #'(arg-id ...) #'rest-tmp #'try-next))
                          (with-syntax-parse ([(arg ...) (fcase-args fc)]
                                              [(arg-parsed::binding-form ...) (fcase-arg-parseds fc)]
                                              [(this-arg-id ...) this-args]
                                              [pred (fcase-pred fc)]
                                              [rhs (fcase-rhs fc)]
                                              [(maybe-match-rest (maybe-bind-rest ...) (maybe-bind-rest-static-info ...))
                                               (cond
                                                 [(syntax-e (fcase-rest-arg fc))
                                                  (define rest-parsed (fcase-rest-arg-parsed fc))
                                                  (with-syntax-parse ([rest::binding-form rest-parsed])
                                                    #`((rest-tmp rest-getter #,rest-parsed #,(fcase-rest-arg fc))
                                                       ((define-values (rest.bind-id ...) (rest-getter)))
                                                       ((define-static-info-syntax/maybe rest.bind-id (#%ref-result (rest.bind-static-info ...)))
                                                        ...)))]
                                                 [else
                                                  #'(#f () ())])])
                            #`(let ([try-next (lambda () #,(loop (cdr same)))])
                                #,(wrap-adapted-arguments
                                   #`(nested-bindings
                                      #,function-name
                                      try-next
                                      argument-binding-failure
                                      (this-arg-id arg-parsed arg #f)
                                      ...
                                      maybe-match-rest
                                      (begin
                                        (arg-parsed.binder-id this-arg-id arg-parsed.data) ...
                                        maybe-bind-rest ...
                                        (begin
                                          (define-static-info-syntax/maybe arg-parsed.bind-id arg-parsed.bind-static-info ...)
                                          ...)
                                        ...
                                        maybe-bind-rest-static-info ...
                                        (add-contract-check
                                         #,function-name
                                         pred
                                         (rhombus-expression (group rhs))))))))]))])))))

  (define (maybe-add-function-result-definition name static-infoss defns)
    (define (same-expression? a b)
      (cond
        [(identifier? a) (and (identifier? b)
                              (free-identifier=? a b))]
        [(syntax? a) (same-expression? (syntax-e a) b)]
        [(syntax? b) (same-expression? a (syntax-e b))]
        [(pair? a) (and (pair? b)
                        (same-expression? (car a) (car b))
                        (same-expression? (cdr a) (cdr b)))]
        [else (equal? a b)]))
    (cond
      [(and (pair? static-infoss)
            (pair? (syntax-e (car static-infoss)))
            (for/and ([static-infos (in-list (cdr static-infoss))])
              (same-expression? (car static-infoss) static-infos)))
       (cons
        #`(define-static-info-syntax #,name (#%call-result #,(car static-infoss)))
        defns)]
      [else defns]))

  ;; returns (listof (cons n (listof fcase)))
  ;; where `n` is the argument count, and a negative
  ;; `n` means "-(n+1) or more"; although the `n`s
  ;; can be in any order, the `fcase`s are kept in the same
  ;; order within the group for one `n`
  (define (group-by-counts fcases)
    ;; if there is any rest clause, then other clauses
    ;; whose arity overlaps needs to be merged; a rest
    ;; clause requiring at least N arguments will merge
    ;; with any clause that accepts N or more
    (define rest-min
      (for/fold ([rest-min #f]) ([fc (in-list fcases)])
        (cond
          [(syntax-e (fcase-rest-arg fc))
           (define n (length (fcase-args fc)))
           (if rest-min (min rest-min n) n)]
          [else rest-min])))
    (define ht
      (for/fold ([ht #hasheqv()]) ([fc (in-list fcases)])
        (define n (length (fcase-args fc)))
        (define key (if (and rest-min (>= n rest-min))
                        (- (add1 rest-min))
                        n))
        (hash-set ht key (cons fc (hash-ref ht key '())))))
    (for/list ([(key sames) (in-hash ht)])
      (cons key (reverse sames))))

  (define (find-matching-case n same)
    (define find-n (if (negative? n) (- (add1 n)) n))
    (for/or ([fc (in-list same)])
      (define fc-n (length (fcase-args fc)))
      (and (eqv? find-n fc-n)
           fc)))

  ;; when a clause that expects n' (or more) arguments is merged
  ;; with a clause that expects n or more arguments (so n <= n'), then
  ;; the rest argument needs to be unpacked to extra arguments
  (define (adapt-arguments-for-count fc n arg-ids-stx rest-tmp try-next)
    (define base-f-n (length (fcase-args fc)))
    (define f-n (if (syntax-e (fcase-rest-arg fc))
                    (- (add1 base-f-n))
                    base-f-n))
    (cond
      [(eqv? n f-n) (values arg-ids-stx values)]
      [else
       (unless (negative? n) (error "assert failed in wrap-adapted"))
       (define base-n (- (add1 n)))
       (define arg-ids (syntax->list arg-ids-stx))
       (define new-arg-ids (append arg-ids
                                   (generate-temporaries (list-tail (fcase-args fc) base-n))))
       (values
        new-arg-ids
        (lambda (body)
          (let loop ([new-arg-ids (list-tail new-arg-ids base-n)])
            (cond
              [(null? new-arg-ids)
               (if (negative? f-n)
                   body
                   #`(if (null? #,rest-tmp)
                         (let ()
                           #,body)
                         (#,try-next)))]
              [else
               #`(if (pair? #,rest-tmp)
                     (let ([#,(car new-arg-ids) (car #,rest-tmp)])
                       (let ([rest-tmp (cdr #,rest-tmp)])
                         #,(loop (cdr new-arg-ids))))
                     (#,try-next))]))))])))

(define (argument-binding-failure who val binding)
  (raise-binding-failure who "argument" val binding))

(define (cases-failure who rest-args . base-args)
  (define args (append base-args rest-args))
  (apply error who
         (apply string-append "no matching case for arguments\n"
                "  arguments...:"
                (for/list ([arg (in-list args)])
                  "\n   ~e"))
         args))

(define-syntax (add-contract-check stx)
  (syntax-parse stx
    [(_ name pred e)
     (cond
       [(syntax-e #'pred)
        #`(let ([result e])
            (if (pred e)
                e
                (result-failure 'name e)))]
       [else #'e])]))

(define (result-failure who val)
  (error who
         (string-append "result does not match contract\n"
                        "  result: ~v\n")
         val))

(begin-for-syntax
  (define-syntax-class :kw-expression
    #:datum-literals (op block group)
    (pattern (group kw:keyword (block (group e ...)))
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
                                       (list kw parsed)
                                       (list parsed)))
     (define e (datum->syntax (quote-syntax here)
                              (cons rator #'(arg-form ... ...))
                              (span-srcloc rator #'head)
                              #'head))
     (define result-static-infos (or (syntax-local-static-info rator #'#%call-result)
                                     #'()))
     (values (wrap-static-info* e result-static-infos)
             #'tail)]))
