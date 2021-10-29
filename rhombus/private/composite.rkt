#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt"
         "binding.rkt"
         "static-info.rkt"
         "ref-result-key.rkt"
         (submod "annotation.rkt" for-class))

;; `make-composite-binding-transformer` is mostly generic with respect
;; to a composite datatype, but the `rest` support is currently
;; hardwired to lists.

(provide (for-syntax make-composite-binding-transformer
                     make-rest-match))

(define-for-syntax (make-composite-binding-transformer predicate     ; predicate for the composite value
                                                       accessors     ; one accessor per component
                                                       static-infoss ; one set of static info per component
                                                       #:steppers [steppers #f] ; a sequence of `cdr`s for lists
                                                       #:static-infos [composite-static-infos #'()] ; for the composite value
                                                       #:accessor->info? [accessor->info? #f] ; extend composite info?
                                                       #:ref-result-info? [ref-result-info? #f]
                                                       #:rest-accessor [rest-accessor #f]) ; for a list "rest"
  (lambda (tail [rest-arg #f])
    (syntax-parse tail
      [(form-id ((~datum parens) a::binding ...) . new-tail)
       #:with (a-parsed::binding-form ...) #'(a.parsed ...)
       ;; `rest-a` will have either 0 items or 1 item
       #:with (rest-a::binding ...) (if rest-arg (list rest-arg) null)
       #:with (rest-a-parsed::binding-form ...) #'(rest-a.parsed ...)
       (define as (syntax->list #'(a ...)))
       (unless (= (length as) (length accessors))
         (raise-syntax-error #f
                             (format (string-append "pattern arguments not the expected number\n"
                                                    "  expected: ~a\n"
                                                    "  given: ~a")
                                     (length accessors)
                                     (length as))
                             (syntax/loc #'form-id
                               #'(group form-id (parens a ...)))))
       (values
        (binding-form
         #'composite-infoer
         #`(#,predicate
            #,composite-static-infos
            #,steppers
            #,accessors
            #,static-infoss
            (a-parsed.infoer-id ... )
            (a-parsed.data ...)
            #,accessor->info? #,ref-result-info?
            #,(and rest-arg
                   #`(#,rest-accessor
                      rest-a-parsed.infoer-id ...
                      rest-a-parsed.data ...))))
        #'new-tail)])))

(require (for-syntax racket/pretty))

(define-syntax (composite-infoer stx)
  (syntax-parse stx
    [(_ static-infos (predicate (composite-static-info ...)
                                steppers accessors ((static-info ...) ...)
                                (infoer-id ...) (data ...)
                                accessor->info? ref-result-info?
                                rest-data))
     #:with (arg-static-infos ...) (for/list ([accessor (in-list (syntax->list #'accessors))])
                                     (or (and (syntax-e #'accessor->info?)
                                              (static-info-lookup #'static-infos accessor))
                                         (and (syntax-e #'ref-result-info?)
                                              (static-info-lookup #'static-infos #'#%ref-result))
                                         '()))
     #:with (a-impl::binding-impl ...) #'((infoer-id (static-info ... . arg-static-infos) data) ...)
     #:with (a-info::binding-info ...) #'(a-impl.info ...)

     (define-values (new-rest-data rest-static-infos rest-bind-infos rest-name-id)
       (syntax-parse #'rest-data
         [#f (values #'#f #'() #'() #'rest)]
         [(rest-accessor rest-infoer-id rest-a-data)
          #:with rest-impl::binding-impl #'(rest-infoer-id () rest-a-data)
          #:with rest-info::binding-info #'rest-impl.info
          #:with (rest-tmp-id) (generate-temporaries #'(rest-info.name-id))
          (values #`(rest-tmp-id rest-accessor rest-info)
                  #'rest-info.static-infos
                  (syntax-parse #'rest-info.bind-infos
                    [((bind-id bind-static-info ...) ...)
                     #:with ref-static-infos (or (static-info-lookup #'static-infos #'#%ref-result)
                                                 '())
                     #'((bind-id (#%ref-result (bind-static-info ... . ref-static-infos))) ...)])
                  #'rest-info.name-id)]))

     (define all-composite-static-infos
       (let* ([composite-static-infos #'(composite-static-info ... . static-infos)]
              [composite-static-infos (if (or (null? (syntax-e rest-static-infos))
                                              (not (null? (syntax-e #'accessors))))
                                          composite-static-infos
                                          #`((#%ref-result #,rest-static-infos) . #,composite-static-infos))]
              [composite-static-infos (cond
                                        [(syntax-e #'accessor->info?)
                                         #`(#,@(for/list ([accessor (syntax->list #'accessors)]
                                                          [static-infos (syntax->list #'(a-info.static-infos ...))])
                                                 #`(#,accessor #,static-infos))
                                            . #,composite-static-infos)]
                                        [else composite-static-infos])]
              [composite-static-infos (cond
                                        [(and (syntax-e #'ref-result-info?)
                                              (static-info-lookup #'static-infos #'#%ref-result))
                                         => (lambda (ref-result)
                                              #`((#%ref-result #,ref-result)
                                                 . #,composite-static-infos))]
                                        [else composite-static-infos])])
         composite-static-infos))
     (binding-info #'composite
                   all-composite-static-infos
                   #`((a-info.bind-id a-info.bind-static-info ...) ... ... #,@rest-bind-infos)
                   #'composite-matcher
                   #'composite-binder
                   #`(predicate steppers accessors #,(generate-temporaries #'(a-info.name-id ...))
                                (a-info.name-id ...) (a-info.matcher-id ...) (a-info.binder-id ...) (a-info.data ...)
                                #,new-rest-data))]))

(define-syntax (composite-matcher stx)
  (syntax-parse stx
    [(_ c-arg-id
        (predicate steppers accessors tmp-ids
                   name-ids matcher-ids binder-ids datas
                   rest-data)
        IF success-expr fail-expr)
     #`(IF (predicate c-arg-id)
           #,(let loop ([c-arg-id #'c-arg-id]
                        [steppers (syntax->list #'steppers)]
                        [accessors (syntax->list #'accessors)]
                        [tmp-ids (syntax->list #'tmp-ids)]
                        [name-ids (syntax->list #'name-ids)]
                        [matcher-ids (syntax->list #'matcher-ids)]
                        [datas (syntax->list #'datas)])
               (cond
                 [(null? name-ids)
                  (syntax-parse #'rest-data
                    [#f #`(IF #t success-expr fail-expr)]
                    [(rest-tmp-id rest-accessor rest-info)
                     #`(begin
                         (define rest-tmp-id #,(make-rest-match c-arg-id #'rest-accessor #'rest-info #'(lambda (arg) #f)))
                         (IF rest-tmp-id
                             success-expr
                             fail-expr))])]
                 [else
                  (define new-c-arg-id (if steppers
                                           (car (generate-temporaries '(c-arg-id)))
                                           c-arg-id))
                  #`(begin
                      #,@(if steppers
                             #`((define #,new-c-arg-id (#,(car steppers) #,c-arg-id)))
                             #'())
                      (define #,(car tmp-ids) (let ([#,(car name-ids) (#,(car accessors) #,new-c-arg-id)])
                                                #,(car name-ids)))
                      (#,(car matcher-ids) #,(car tmp-ids) #,(car datas)
                       IF
                       #,(loop new-c-arg-id
                               (and steppers (cdr steppers)) (cdr accessors)
                               (cdr tmp-ids) (cdr name-ids) (cdr matcher-ids)
                               (cdr datas))
                       fail-expr))]))
           fail-expr)]))

(define-syntax (composite-binder stx)
  (syntax-parse stx
    [(_ c-arg-id (predicate steppers accessors (tmp-id ...)
                            name-ids matcher-ids (binder-id ...) (data ...)
                            rest-data))
     #`(begin
         (binder-id tmp-id data)
         ...
         #,@(syntax-parse #'rest-data
              [#f #'()]
              [(rest-tmp-id rest-accessor rest-info)
               #:with rest::binding-info #'rest-info
               #'((define-values (rest.bind-id ...) (rest-tmp-id)))]))]))

;; ------------------------------------------------------------

;; a match result for a "rest" match is a function that gets
;; lists of results; the binder step for each element is delayed
;; until the whole list is found to match
(define-for-syntax (make-rest-match c-arg-id accessor rest-info fail)
  (syntax-parse rest-info
    [rest::binding-info
     #`(get-rest-getters '(rest.bind-id ...)
                         (let ([rest.name-id (#,accessor #,c-arg-id)])
                           rest.name-id)
                         (lambda (arg-id)
                           (rest.matcher-id arg-id rest.data
                            if/blocked
                            (lambda ()
                              (rest.binder-id arg-id rest.data)
                              (values rest.bind-id ...))
                            (#,fail arg-id))))]))

(define-syntax-rule (if/blocked tst thn els)
  (if tst (let () thn) els))

;; run-time support for "rest" matching
(define (get-rest-getters rest-syms rest-val get-one-getter)
  (and (list? rest-val)
       (let loop ([rest-val rest-val] [accum null])
         (cond
           [(null? rest-val) (build-overall-getter rest-syms accum)]
           [else
            (define getter (get-one-getter (car rest-val)))
            (and getter
                 (loop (cdr rest-val) (cons getter accum)))]))))

;; more run-time support for "rest" matching, creates the function
;; that is called after a successful match to get all the results
(define (build-overall-getter rest-syms accum)                                             
  (cond
    [(and (pair? rest-syms) (null? (cdr rest-syms)))
     ;; simple case: single-values
     (lambda ()
       (for/list ([getter (reverse accum)])
         (getter)))]
    [else
     ;; each getter returns multiple values
     (lambda ()
       (apply
        values
        (let loop ([getters (reverse accum)])
          (cond
            [(null? getters)
             (for/list ([rest-sym (in-list rest-syms)]) null)]
            [else
             (define vals (call-with-values (car getters) list))
             (define rest-valss (loop (cdr getters)))
             (for/list ([val (in-list vals)]
                        [rest-vals (in-list rest-valss)])
               (cons val rest-vals))]))))]))
