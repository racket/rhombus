#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/unsafe/undefined)

(provide lambda/kwrest
         case-lambda/kwrest)

;; ---------------------------------------------------------

(define-syntax lambda/kwrest
  (lambda (stx)
    (syntax-parse stx
      ;; This case is unused, because Racket `define` has to recognize
      ;; an immediate, unexpanded `lambda` in order to optimize
      ;; keyword arguments.
      #;
      [(_ #:name name
          #:arity _
          #:method method?
          (~optional (~seq #:rest rest))
          (arg ...)
          b ...)
       (let* ([proc (syntax/loc stx
                      (lambda (~? (arg ... . rest)
                                  (arg ...))
                        b ...))]
              [proc (syntax-property proc 'inferred-name (syntax-e #'name))]
              [proc (if (syntax-e #'method?)
                        (syntax-property proc 'method-arity-error #t)
                        proc)])
         proc)]
      [(_ #:name name
          #:arity arity
          #:method method?
          (~optional (~seq #:rest rest))
          #:kwrest kwrest
          ;; assumption: default values are simple enough to be duplicated
          ((~alt (~seq kw:keyword (~or* [kw-arg kw-default] kw-arg))
                 (~or* [arg default] arg))
           ...)
          b ...)
       ;; assumption: optional by-position arguments must appear after
       ;; mandatory by-position arguments
       (define args (syntax->list #'(arg ...)))
       (define defaults (syntax->list #'((~? default) ...)))
       (define non-rest-max (length args))
       (define non-rest-min (- non-rest-max (length defaults)))
       (define-values (kw-proc-claws proc-claws)
         (for/lists (kw-proc-claws proc-claws)
                    ([n (in-inclusive-range non-rest-min non-rest-max)])
           (cond
             [(eqv? n non-rest-max)
              (values #`(~? [(ks vs #,@args . rest)
                             (kw-entry (keyword-lists->hash ks vs) #,@args rest)]
                            [(ks vs #,@args)
                             (kw-entry (keyword-lists->hash ks vs) #,@args)])
                      #`(~? (~? [(#,@args . rest)
                                 (entry '#hashalw() kw-default ... #,@args rest)]
                                [(#,@args)
                                 (entry '#hashalw() kw-default ... #,@args)])
                            [(~? (#,@args . rest)
                                 (#,@args))
                             (raise-should-not-reach-error 'name)]))]
             [else
              (define given-args
                (for/list ([arg (in-list args)]
                           [_ (in-range n)])
                  arg))
              (define unsupplied-args
                (list-tail defaults (- n non-rest-min)))
              (define maybe-rest-arg
                (if (attribute rest) (list #''()) '()))
              (values #`[(ks vs #,@given-args)
                         (kw-entry (keyword-lists->hash ks vs) #,@given-args #,@unsupplied-args #,@maybe-rest-arg)]
                      #`[(#,@given-args)
                         (~? (entry '#hashalw() kw-default ... #,@given-args #,@unsupplied-args #,@maybe-rest-arg)
                             (raise-should-not-reach-error 'name))])])))
       #`(let ([entry (lambda (~? (kwrest kw-arg ... arg ... rest)
                                  (kwrest kw-arg ... arg ...))
                        b ...)])
           (let ([kw-entry (lambda (~? (kwrest arg ... rest)
                                       (kwrest arg ...))
                             (let*-values ([(kw-arg kwrest)
                                            ;; `unsafe-undefined` cannot be the result of a safe expression
                                            (~? (let ([val (hash-ref kwrest 'kw unsafe-undefined)])
                                                  (if (eq? val unsafe-undefined)
                                                      (values kw-default kwrest)
                                                      (values val (hash-remove kwrest 'kw))))
                                                ;; guarded by reduced arity
                                                (values (hash-ref kwrest 'kw) (hash-remove kwrest 'kw)))]
                                           ...)
                               (~? (entry kwrest kw-arg ... arg ... rest)
                                   (entry kwrest kw-arg ... arg ...))))])
             #,(make-procedure-reduce-keyword-arity-mask
                #`(make-keyword-procedure
                   #,(syntax-property
                      (quasisyntax/loc stx
                        (case-lambda #,@kw-proc-claws))
                      'inferred-name (syntax-e #'name))
                   #,(syntax-property
                      (quasisyntax/loc stx
                        (case-lambda #,@proc-claws))
                      'inferred-name (syntax-e #'name)))
                #'arity
                #'method?)))])))

(define-syntax case-lambda/kwrest
  (lambda (stx)
    (syntax-parse stx
      [(_ #:name name
          #:arity _
          #:method method?
          [(~optional (~seq #:rest rest))
           (arg ...)
           b ...]
          ...)
       (let* ([proc (syntax/loc stx
                      (case-lambda
                        [(~? (arg ... . rest)
                             (arg ...))
                         b ...]
                        ...))]
              [proc (syntax-property proc 'inferred-name (syntax-e #'name))]
              [proc (if (syntax-e #'method?)
                        (syntax-property proc 'method-arity-error #t)
                        proc)])
         proc)]
      ;; assumption: all clauses must accept `#:kwrest`
      ;; We don't do anything fancy here, because dispatch code is
      ;; generated by `fun` itself.
      [(_ #:name name
          #:arity arity
          #:method method?
          (~and claw
                [(~optional (~seq #:rest rest))
                 #:kwrest kwrest
                 (arg ...)
                 b ...])
          ...)
       #:with (entry ...) (generate-temporaries
                           (for/list ([_ (in-list (syntax->list #'(claw ...)))])
                             'entry))
       #`(let ([entry (lambda (~? (kwrest arg ... rest)
                                  (kwrest arg ...))
                        b ...)]
               ...)
           #,(make-procedure-reduce-keyword-arity-mask
              #`(make-keyword-procedure
                 #,(syntax-property
                    (syntax/loc stx
                      (case-lambda
                        (~? [(ks vs arg ... . rest)
                             (entry (keyword-lists->hash ks vs) arg ... rest)]
                            [(ks vs arg ...)
                             (entry (keyword-lists->hash ks vs) arg ...)])
                        ...))
                    'inferred-name (syntax-e #'name))
                 #,(syntax-property
                    (syntax-parse #'arity
                      [(_ (_ _ ...) _)
                       (syntax/loc stx
                         (case-lambda
                           [(~? (arg ... . rest)
                                (arg ...))
                            (raise-should-not-reach-error 'name)]
                           ...))]
                      [_
                       (syntax/loc stx
                         (case-lambda
                           (~? [(arg ... . rest)
                                (entry '#hashalw() arg ... rest)]
                               [(arg ...)
                                (entry '#hashalw() arg ...)])
                           ...))])
                    'inferred-name (syntax-e #'name)))
              #'arity
              #'method?))])))

(define-for-syntax (make-procedure-reduce-keyword-arity-mask proc arity method?-stx)
  (define p
    (syntax-parse arity
      [(_ () #f) proc]
      [(mask required-kws allowed-kws)
       #`(procedure-reduce-keyword-arity-mask #,proc 'mask 'required-kws 'allowed-kws)]))
  (if (syntax-e method?-stx)
      #`(procedure->method #,p)
      p))

;; ---------------------------------------------------------

;; keyword-lists->hash : (Listof Keyword) (Listof V) -> (Hashof Keyword V)
(define (keyword-lists->hash ks vs)
  (for/hashalw ([k (in-list ks)]
                [v (in-list vs)])
    (values k v)))

(define (raise-should-not-reach-error name)
  (raise-arguments-error name
                         (string-append "should not reach this path"
                                        ";\n procedure requires keyword arguments")))
