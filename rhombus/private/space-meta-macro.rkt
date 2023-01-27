#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "expose.rkt")
         syntax/parse
         racket/symbol
         enforest
         enforest/property
         enforest/operator
         enforest/transformer
         enforest/transformer-result
         enforest/proc-name
         (for-template "enforest.rkt")
         "introducer.rkt"
         "name-path-op.rkt"
         "pack.rkt"
         (submod "syntax-class-primitive.rkt" for-quasiquote)
         (submod "syntax-class-primitive.rkt" for-syntax-class)
         (for-syntax racket/base)
         "implicit.rkt" ;; needed for `$%body`
         "name-root.rkt"
         "space.rkt"
         "realm.rkt"
         "parse.rkt"
         "forwarding-sequence.rkt"
         "space-meta-clause.rkt"
         (submod "space-meta-clause-primitive.rkt" for-space-meta-macro)
         (submod "namespace.rkt" for-exports))

(provide enforest-meta
         transform-meta
         (for-space rhombus/space
                    space_meta_clause))

(define-space-syntax space_meta_clause
  (space-syntax rhombus/space_meta_clause))

(define-syntax (enforest-meta stx)
  (syntax-parse stx
    #:datum-literals (group)
    [(_  name:identifier names clauses)
     #`(rhombus-mixed-nested-forwarding-sequence (enforest-meta-finish [#,stx name #t . names]) rhombus-meta-enforest
                                                 (enforest-meta-body-step . clauses))]))

(define-syntax (transform-meta stx)
  (syntax-parse stx
    #:datum-literals (group)
    [(_  name:identifier names clauses)
     #`(rhombus-mixed-nested-forwarding-sequence (enforest-meta-finish [#,stx name #f . names]) rhombus-meta-enforest
                                                 (enforest-meta-body-step . clauses))]))
  
(define-syntax enforest-meta-body-step
  (lambda (stx)
    (syntax-parse stx
      [(_ form . rest)
       #:with clause::space-meta-clause (syntax-local-introduce #'form)
       (syntax-parse (syntax-local-introduce #'clause.parsed)
         #:datum-literals (group parsed)
         [((group (parsed p)) ...)
          #`(begin p ... (enforest-meta-body-step . rest))]
         [(form ...)
          #`(enforest-meta-body-step form ... . rest)])]
      [(_ form . rest)
       #`(rhombus-top-step
          enforest-meta-body-step
          #f
          ()
          form . rest)]
      [(_) #'(begin)])))
             
(define-syntax enforest-meta-finish
  (lambda (stx)
    (syntax-parse stx
      [(_ [orig-stx
           meta-name enforest?
           name base-stx scope-stx
           space-path-name make-prefix-operator make-infix-operator make-prefix+infix-operator]
          exports
          option
          ...)
       (define options (parse-space-meta-clause-options #'orig-stx (syntax-e #'transformer?) #'(option ...)))
       (define class-name (hash-ref options '#:syntax_class #'#f))
       (define prefix-more-class-name (hash-ref options '#:syntax_class_prefix_more #'#f))
       (define infix-more-class-name (hash-ref options '#:syntax_class_infix_more #'#f))
       (define desc (hash-ref options '#:desc #'"form"))
       (define desc-operator (hash-ref options '#:operator_desc #'"operator"))
       (define macro-result (hash-ref options '#:parsed_checker #'check-syntax))
       (define identifier-transformer (hash-ref options '#:identifier_transformer #'values))
       (define expose (make-expose #'scope-stx #'base-stx))
       (define exs (parse-exports #'(combine-out . exports)))
       (check-distinct-exports (exports->names exs)
                               class-name prefix-more-class-name infix-more-class-name
                               #'orig-stx)
       (cond
         [(syntax-e #'enforest?)
          #`(begin
              (define-name-root #,(expose #'meta-name)
                #:fields
                (#,@(filter-missing
                     #`([#,class-name _class-name]
                        [#,prefix-more-class-name _prefix-more-class-name]
                        [#,infix-more-class-name _infix-more-class-name]))
                 . #,exs))
              (define in-new-space (make-interned-syntax-introducer/add 'space-path-name))
              (property new-prefix-operator prefix-operator)
              (property new-infix-operator infix-operator)
              (struct new-prefix+infix-operator (prefix infix)
                #:property prop:new-prefix-operator (lambda (self) (new-prefix+infix-operator-prefix self))
                #:property prop:new-infix-operator (lambda (self) (new-prefix+infix-operator-infix self)))
              (define-rhombus-enforest
                #:syntax-class :base
                #:prefix-more-syntax-class :prefix-more
                #:infix-more-syntax-class :infix-more
                #:desc #,desc
                #:operator-desc #,desc-operator
                #:in-space in-new-space
                #:prefix-operator-ref new-prefix-operator-ref
                #:infix-operator-ref new-infix-operator-ref
                #:check-result #,macro-result
                #:make-identifier-form #,identifier-transformer)
              (define-syntax _class-name (make-syntax-class #':base
                                                            #:kind 'group
                                                            #:fields #'((parsed parsed parsed 0 unpack-term*))))
              (define-syntax _prefix-more-class-name (make-syntax-class #':prefix-more
                                                                        #:kind 'group
                                                                        #:fields #'((parsed parsed #f 0 unpack-term*)
                                                                                    (tail #f tail tail unpack-tail-list*))
                                                                        #:arity 2))
              (define-syntax _infix-more-class-name (make-syntax-class #':infix-more
                                                                       #:kind 'group
                                                                       #:fields #'((parsed parsed #f 0 unpack-term*)
                                                                                   (tail #f tail tail unpack-tail-list*))
                                                                       #:arity 2))
              (define make-prefix-operator (make-make-prefix-operator new-prefix-operator))
              (define make-infix-operator (make-make-infix-operator new-infix-operator))
              (define make-prefix+infix-operator new-prefix+infix-operator))]
         [else
          #`(begin
              (define-name-root #,(expose #'meta-name)
                #:root (space-syntax space-path-name)
                #:fields
                #,(filter-missing
                   #`([#,class-name _class-name])))
              (define in-new-space (make-interned-syntax-introducer/add 'space.name))
              (property new-transformer transformer)
              (define-rhombus-transform
                #:syntax-class :base
                #:desc #,desc
                #:in-space in-new-space
                #:transformer-ref new-transformer-ref
                #:check-result #,macro-result)
              (define-syntax _class-name (rhombus-syntax-class 'group #':base #'((parsed parsed 0 unpack-term*)) #f #f))
              (define make-transformer (make-make-transformer new-transformer)))])])))

(define-for-syntax (filter-missing flds)
  (for/list ([fld (in-list (syntax->list flds))]
             #:when (syntax-parse fld
                      [[#f . _] #f]
                      [_ #t]))
    fld))

(define ((make-make-prefix-operator new-prefix-operator) name prec protocol proc)
  (new-prefix-operator
   name
   prec
   protocol
   (cond
     [(eq? protocol 'automatic) proc]
     [else
      (procedure-rename
       (lambda (tail)
         (finish (lambda () (syntax-parse tail
                              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)]))
                 proc))
       (object-name proc))])))

(define ((make-make-infix-operator new-infix-operator) name prec protocol proc assc)
  (new-infix-operator
   name
   prec
   protocol
   (cond
     [(eq? protocol 'automatic) proc]
     [else
      (procedure-rename
       (lambda (form1 tail)
         (finish
          (lambda () (syntax-parse tail
                       [(head . tail) (proc form1 (pack-tail #'tail #:after #'head) #'head)]))
          proc))
       (object-name proc))])
   assc))

(define ((make-make-transformer new-transformer) proc)
  (new-transformer
   (lambda (stx)
     (define r (syntax-parse stx
                 [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (check-syntax r proc)
     r)))

(define (finish thunk proc)
  (define-values (form new-tail)
    (call-with-values
     thunk
     (case-lambda
       [(form new-tail) (values form new-tail)]
       [(form) (values form #'(group))])))
  (values form
          (unpack-tail new-tail proc #f)))

(define (check-syntax form proc)
  (unless (syntax? form)
    (raise-result-error* (proc-name proc) rhombus-realm "Syntax" form))
  form)

(define-for-syntax (check-distinct-exports ex-ht
                                           class-name prefix-more-class-name infix-more-class-name
                                           orig-stx)
  (define (check id what)
    (when (and (syntax-e id)
               (hash-ref ex-ht (syntax-e id) #f))
      (raise-syntax-error #f
                          (string-append "exported name conflicts with exported " what)
                          orig-stx
                          (hash-ref ex-ht (syntax-e id) #f))))
  (check class-name "syntax class name")
  (check class-name "prefix-more syntax class name")
  (check class-name "infix-more syntax class name"))
