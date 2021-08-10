#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt"
         "binding.rkt"
         "static-info.rkt"
         "ref-result-key.rkt"
         "bind-input-key.rkt"
         (submod "contract.rkt" for-struct))

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
       (define new-bind-ids (apply
                             append
                             (append
                              ;; strip `#%bind-input` from any component static info, but in that case,
                              ;; add information from `static-infos`
                              (for/list ([sis (in-list static-infoss)]
                                         [matcher-id (in-list (syntax->list #'(a-parsed.matcher-id ...)))]
                                         [bind-ids-stx (in-list (syntax->list #'(a-parsed.bind-ids ...)))])
                                (extend-bind-input sis (syntax->list bind-ids-stx)))
                              ;; for a "rest" pattern, push all static info under `#%ref-result`,
                              ;; since each "rest" pattern variable turns into a list
                              (for/list ([static-bind-ids-stx (in-list (syntax->list #'(rest-a-parsed.bind-ids ...)))])
                                (syntax-parse static-bind-ids-stx
                                  [((bind-id . static-infos) ...)
                                   (syntax->list #'((bind-id (#%ref-result static-infos)) ...))])))))
       (define all-composite-static-infos
         (let* ([composite-static-infos composite-static-infos]
                [composite-static-infos (if (and rest-arg
                                                 (null? accessors))
                                            #`((#%ref-result rest-a-parsed.static-infos ...) . #,composite-static-infos)
                                            composite-static-infos)]
                [composite-static-infos (if accessor->info?
                                            #`(#,@(for/list ([accessor accessors]
                                                             [static-infos (syntax->list #'(a-parsed.static-infos ...))])
                                                    #`(#,accessor #,static-infos))
                                               . #,composite-static-infos)
                                            composite-static-infos)])
           composite-static-infos))
       (values
        (binding-form
         #'composite
         all-composite-static-infos
         new-bind-ids
         #'composite-matcher
         #'composite-binder
         #`(#,predicate
            #,steppers
            #,(if rest-arg (append accessors (list rest-accessor)) accessors)
            #,(generate-temporaries #'(a-parsed.arg-id ... rest-a-parsed.arg-id ...))
            (a-parsed.arg-id ... rest-a-parsed.arg-id ...)
            (a-parsed.matcher-id ... rest-a-parsed.matcher-id ...)
            (a-parsed.binder-id ... rest-a-parsed.binder-id ...)
            (a-parsed.data ... rest-a-parsed.data ...)
            #,(and rest-arg
                   ;; so we know what results to collect from each "rest" element match:
                   #`(rest-a-parsed.bind-id ... ...))))
        #'new-tail)])))

(require (for-syntax racket/pretty))

(define-syntax (composite-matcher stx)
  (syntax-parse stx
    [(_ c-arg-id
        (predicate steppers accessors tmp-ids
                   arg-ids matcher-ids binder-ids datas
                   rest-ids)
        IF success-expr fail-expr)
     #`(IF (predicate c-arg-id)
           #,(let loop ([c-arg-id #'c-arg-id]
                        [steppers (syntax->list #'steppers)]
                        [accessors (syntax->list #'accessors)]
                        [tmp-ids (syntax->list #'tmp-ids)]
                        [arg-ids (syntax->list #'arg-ids)]
                        [matcher-ids (syntax->list #'matcher-ids)]
                        [binder-ids (syntax->list #'binder-ids)]
                        [datas (syntax->list #'datas)])
               (cond
                 [(null? arg-ids)
                  #`(IF #t success-expr fail-expr)]
                 [(and (syntax-e #'rest-ids)
                       (null? (cdr arg-ids)))
                  #`(begin
                      (define #,(car tmp-ids) #,(make-rest-match c-arg-id #'rest-ids (car accessors)
                                                                 (car arg-ids) (car matcher-ids) (car binder-ids)
                                                                 (car datas)
                                                                 #'(lambda (arg) #f)))
                      (IF #,(car tmp-ids)
                          success-expr
                          fail-expr))]
                 [else
                  (define new-c-arg-id (if steppers
                                           (car (generate-temporaries '(c-arg-id)))
                                           c-arg-id))
                  #`(begin
                      #,@(if steppers
                             #`((define #,new-c-arg-id (#,(car steppers) #,c-arg-id)))
                             #'())
                      (define #,(car tmp-ids) (let ([#,(car arg-ids) (#,(car accessors) #,new-c-arg-id)])
                                                #,(car arg-ids)))
                      (#,(car matcher-ids) #,(car tmp-ids) #,(car datas)
                       IF
                       #,(loop new-c-arg-id
                               (and steppers (cdr steppers)) (cdr accessors)
                               (cdr tmp-ids) (cdr arg-ids) (cdr matcher-ids) (cdr binder-ids)
                               (cdr datas))
                       fail-expr))]))
           fail-expr)]))

(define-syntax (composite-binder stx)
  (syntax-parse stx
    [(_ c-arg-id (predicate steppers accessors (tmp-id ...)
                            (arg-id ...) matcher-ids (binder-id ...) (data ...)
                            #f))
     #`(begin
         (binder-id tmp-id data)
         ...)]
    [(_ c-arg-id (predicate steppers accessors (tmp-id ... rest-tmp-id)
                            (arg-id ...) matcher-ids (binder-id ... rest-binder-id) (data ... rest-data)
                            (rest-id ...)))
     #`(begin
         (binder-id tmp-id data)
         ...
         (define-values (rest-id ...) (rest-tmp-id)))]))

;; ------------------------------------------------------------

;; a match result for a "rest" match is a function that gets
;; lists of results; the binder step for each element is delayed
;; until the whole list is found to match
(define-for-syntax (make-rest-match c-arg-id rest-ids selector
                                    arg-id matcher-id binder-id
                                    data
                                    fail)
  #`(get-rest-getters '#,rest-ids
                      (let ([#,arg-id (#,selector #,c-arg-id)])
                        #,arg-id)
                      (lambda (arg-id)
                        (#,matcher-id arg-id #,data
                         if/blocked
                         (lambda ()
                           (#,binder-id arg-id #,data)
                           (values . #,rest-ids))
                         (#,fail arg-id)))))

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
