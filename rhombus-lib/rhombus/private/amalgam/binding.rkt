#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/property
                     enforest/operator
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     enforest/syntax-local
                     "introducer.rkt"
                     "annotation-string.rkt"
                     (for-syntax racket/base)
                     "macro-result.rkt"
                     "srcloc.rkt"
                     "realm.rkt")
         "dotted-sequence-parse.rkt"
         "binding-failure.rkt")

(begin-for-syntax
  (provide (property-out binding-prefix-operator)
           (property-out binding-infix-operator)

           binding-transformer

           make-identifier-binding

           :binding-form
           binding-form
           check-binding-result

           :binding-values-form
           binding-values-form

           :binding-impl

           :binding-info
           binding-info

           in-binding-space
           bind-quote

           binding-extension-combine
           disallow-binding-as-namespace-extension

           extract-binding-arity

           (struct-out binding-prefix+infix-operator)))

(provide define-binding-syntax
         raise-binding-failure
         raise-bindings-failure
         always-succeed
         empty-oncer)

(begin-for-syntax
  ;; Single-value bindings, which can be lifted
  (define-syntax-class :binding-single-form
    (pattern (#:single
              infoer-id:identifier
              data)))

  ;; Multiple-value bindings, which can be lowered if arity 1
  (define-syntax-class :binding-multi-form
    (pattern (#:multi
              bind-form
              arity:exact-nonnegative-integer
              infoer-id:identifier
              data)))

  ;; To unpack a binding transformer result, single-value:
  (define-syntax-class :binding-form
    #:attributes (infoer-id data)
    (pattern ::binding-single-form)
    ;; multiple-value bindings are lowered if arity 1, otherwise not allowed
    (pattern multi::binding-multi-form
             #:with ::binding-single-form (do-lower-multi-to-single #'multi)))

  ;; To unpack a binding transformer result, multiple-value:
  (define-syntax-class :binding-values-form
    #:attributes (bind-form arity infoer-id data)
    ;; single-value bindings are lifted
    (pattern single::binding-single-form
             #:with ::binding-multi-form (do-lift-single-to-multi #'single))
    (pattern ::binding-multi-form))

  ;; To call an infoer:
  (define-syntax-class (:binding-impl [arity #f])
    (pattern (~and form (infoer-id . _))
             #:do [(define proc (syntax-local-value* #'infoer-id (lambda (v)
                                                                   (and (procedure? v)
                                                                        v))))
                   (unless proc
                     (raise-syntax-error #f
                                         "cannot find a transformer for an infoer"
                                         #'infoer-id))]
             #:with info (check-binding-info-result
                          (let ([form #'form])
                            (syntax-local-introduce
                             (call-as-transformer
                              #'infoer-id
                              (list (syntax-local-introduce form))
                              syntax-track-origin #f
                              proc)))
                          proc
                          arity)))

  (define-syntax-class :evidence-id-tree
    (pattern _:identifier)
    (pattern (_::evidence-id-tree ...)))

  ;; To unpack a binding infoer result:
  (define-syntax-class :binding-info
    (pattern (annotation-str:string
              ;; access guarded by check in `:binding-impl`, otherwise use `attribute`
              (~or* name-id:identifier
                    (~and name-ids (_:identifier ...)))
              (~and static-infos ((_:identifier _) ...))
              (~and bind-infos ((bind-id:identifier (~and bind-uses (bind-use ...)) (~and bind-static-info (_:identifier _)) ...) ...))
              oncer-id:identifier
              matcher-id:identifier
              evidence-ids::evidence-id-tree
              committer-id:identifier
              binder-id:identifier
              data)))

  (property binding-prefix-operator prefix-operator)
  (property binding-infix-operator infix-operator)

  (define (binding-transformer proc)
    (binding-prefix-operator #f '((default . stronger)) 'macro proc))

  ;; puts pieces together into a single-value binding form
  (define (binding-form infoer-id data)
    (no-srcloc
     #`(#:single #,infoer-id #,data)))

  ;; puts pieces together into a multiple-value binding form
  (define (binding-values-form bind-form arity infoer-id data)
    (let ([bind-form (datum->syntax #f bind-form)])
      (relocate+reraw
       bind-form
       #`(#:multi #,bind-form #,arity #,infoer-id #,data))))

  ;; puts pieces together into a `:binding-info`
  (define (binding-info annotation-str
                        name-id-or-ids
                        static-infos
                        bind-infos
                        oncer-id
                        matcher-id
                        evidence-ids
                        committer-id
                        binder-id
                        data)
    (datum->syntax #f (list annotation-str
                            name-id-or-ids
                            static-infos
                            bind-infos
                            oncer-id
                            matcher-id
                            evidence-ids
                            committer-id
                            binder-id
                            data)))

  (define (make-identifier-binding id)
    (binding-form #'identifier-infoer
                  id))

  (define (check-binding-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [(~or* _::binding-single-form _::binding-multi-form) form]
      [_ (raise-bad-macro-result (proc-name proc) "binding" form)]))

  (define (check-binding-info-result form proc arity)
    (syntax-parse (if (syntax? form) form #'#f)
      [::binding-info
       (define single-str "a single name-id")
       (define (multi-str n)
         (format "a sequence of ~a name-id~a"
                 n
                 (if (eqv? n 1) "" "s")))
       (define (arity-error expected-str got-str)
         (format "expected ~a, got ~a" expected-str got-str))
       (cond
         [(not arity)
          (unless (attribute name-id)
            (define num-ids (length (syntax-e #'name-ids)))
            (raise-arguments-error* (proc-name proc) rhombus-realm
                                    (arity-error single-str (multi-str num-ids))
                                    "syntax object" form))]
         [else
          (cond
            [(attribute name-id)
             (raise-arguments-error* (proc-name proc) rhombus-realm
                                     (arity-error (multi-str arity) single-str)
                                     "syntax object" form)]
            [else
             (define num-ids (length (syntax-e #'name-ids)))
             (unless (eqv? num-ids arity)
               (raise-arguments-error* (proc-name proc) rhombus-realm
                                       (arity-error (multi-str arity) (multi-str num-ids))
                                       "syntax object" form))])])
       form]
      [_ (raise-bad-macro-result (proc-name proc) "binding-info" form)]))

  (define in-binding-space (make-interned-syntax-introducer/add 'rhombus/bind))
  (define-syntax (bind-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/bind) #'id))]))

  (define extension-syntax-property-key (gensym 'extension))
  (define (binding-extension-combine prefix extends dots field-id id)
    (syntax-property id extension-syntax-property-key extends))

  (define (disallow-binding-as-namespace-extension b-info)
    (syntax-parse b-info
      [b::binding-info
       (for ([id (in-list (syntax->list #'(b.bind-id ...)))]
             [uses (in-list (syntax->list #'(b.bind-uses ...)))])
         (when (for/or ([use (in-list (syntax->list uses))])
                 (let ([u (syntax-e use)])
                   (and (pair? u)
                        (eq? (syntax-e (car u)) '#:extends))))
           (raise-syntax-error #f
                               "binding a namespace extension is not allowed in this context"
                               id)))]))

  (define (extract-binding-arity b-parsed)
    (syntax-parse b-parsed
      [_::binding-single-form 1]
      [::binding-multi-form (syntax-e #'arity)])))

(begin-for-syntax
  (struct binding-prefix+infix-operator (prefix infix)
    #:property prop:binding-prefix-operator (lambda (self) (binding-prefix+infix-operator-prefix self))
    #:property prop:binding-infix-operator (lambda (self) (binding-prefix+infix-operator-infix self))))

(define-syntax (identifier-infoer stx)
  (syntax-parse stx
    [(_ static-infos id)
     (let ([prefix (syntax-property #'id extension-syntax-property-key)])
       (with-syntax ([maybe-prefix
                      (if prefix
                          #`([#:extends #,prefix])
                          #'())])
         (binding-info annotation-any-string
                       #'id
                       #'static-infos
                       #'((id ([#:repet ()] . maybe-prefix) . static-infos))
                       #'empty-oncer
                       #'always-succeed
                       #'()
                       #'identifier-commit
                       #'identifier-bind
                       (if prefix
                           #`[id #,prefix]
                           #'id))))]))

(define-syntax (empty-oncer stx)
  (syntax-parse stx
    [(_ _)
     #'(begin)]))

(define-syntax (always-succeed stx)
  (syntax-parse stx
    [(_ _ _ IF success fail)
     #'(IF #t success fail)]))

(define-syntax (identifier-commit stx)
  (syntax-parse stx
    [(_ arg-id () bind-id*)
     #'(begin)]))

(define-syntax (identifier-bind stx)
  (syntax-parse stx
    [(_ arg-id () [bind-id prefix-id])
     (define l (build-definitions/maybe-extension #f #'bind-id #'prefix-id
                                                  #'arg-id
                                                  #:simple-rhs? #t))
     (if (and (pair? l) (null? (cdr l)))
         (car l)
         #`(begin #,@l))]
    [(_ arg-id () bind-id)
     #`(#,(syntax-raw-property #'define-values "def") (bind-id) arg-id)]))

(define-syntax (define-binding-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-binding-space #'name) rhs))]))

(define-for-syntax (raise-values-not-allowed bind-form)
  (raise-syntax-error #f
                      "multiple-value binding not allowed in single-value context"
                      (maybe-respan bind-form)))

(define-for-syntax (do-lift-single-to-multi single-stx)
  (syntax-parse single-stx
    [::binding-single-form
     ;; shortcut: remove indirection
     (cond
       [(free-identifier=? #'infoer-id #'lower-multi-to-single-infoer)
        (with-syntax ([(orig-bind-form orig-infoer orig-data) #'data])
          (binding-values-form #'orig-bind-form
                               1
                               #'orig-infoer
                               #'orig-data))]
       [else
        (binding-values-form (list #'lift-single-to-multi single-stx) ; not used, only for unpacking
                             1
                             #'lift-single-to-multi-infoer
                             #'(infoer-id data))])]))

(define-syntax (lift-single-to-multi stx)
  (syntax-parse stx
    [(_ single)
     (do-lift-single-to-multi #'single)]))

(define-syntax (lift-single-to-multi-infoer stx)
  (syntax-parse stx
    [(_ (up-static-infos) (orig-infoer orig-data))
     #:with orig-impl::binding-impl #'(orig-infoer up-static-infos orig-data)
     #:with orig::binding-info #'orig-impl.info
     (binding-info #'orig.annotation-str
                   #'(orig.name-id)
                   #'orig.static-infos
                   #'orig.bind-infos
                   #'lift-single-to-multi-oncer
                   #'lift-single-to-multi-matcher
                   #'orig.evidence-ids
                   #'lift-single-to-multi-committer
                   #'lift-single-to-multi-binder
                   #'orig)]))

(define-syntax (lift-single-to-multi-oncer stx)
  (syntax-parse stx
    [(_ orig::binding-info)
     #'(orig.oncer-id orig.data)]))

(define-syntax (lift-single-to-multi-matcher stx)
  (syntax-parse stx
    [(_ (arg-id) orig::binding-info IF success failure)
     #'(orig.matcher-id arg-id orig.data IF success failure)]))

(define-syntax (lift-single-to-multi-committer stx)
  (syntax-parse stx
    [(_ (arg-id) evidence-ids orig::binding-info)
     #'(orig.committer-id arg-id evidence-ids orig.data)]))

(define-syntax (lift-single-to-multi-binder stx)
  (syntax-parse stx
    [(_ (arg-id) evidence-ids orig::binding-info)
     #'(orig.binder-id arg-id evidence-ids orig.data)]))

(define-for-syntax (do-lower-multi-to-single multi-stx)
  (syntax-parse multi-stx
    [::binding-multi-form
     ;; shortcut: remove indirection
     ;; expose the original infoer, important for certain optimizations
     (cond
       [(free-identifier=? #'infoer-id #'lift-single-to-multi-infoer)
        (with-syntax ([(orig-infoer orig-data) #'data])
          (binding-form #'orig-infoer
                        #'orig-data))]
       [else
        (unless (eqv? (syntax-e #'arity) 1)
          (raise-values-not-allowed #'bind-form))
        (binding-form #'lower-multi-to-single-infoer
                      #'(bind-form infoer-id data))])]))

#;
(define-syntax (lower-multi-to-single stx)
  (syntax-parse stx
    [(_ multi)
     (do-lower-multi-to-single #'multi)]))

(define-syntax (lower-multi-to-single-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos (orig-bind-form orig-infoer orig-data))
     #:with (~var orig-impl (:binding-impl 1)) #'(orig-infoer (up-static-infos) orig-data)
     #:with orig::binding-info #'orig-impl.info
     (binding-info #'orig.annotation-str
                   (with-syntax ([(name-id) #'orig.name-ids])
                     #'name-id)
                   #'orig.static-infos
                   #'orig.bind-infos
                   #'lower-multi-to-single-oncer
                   #'lower-multi-to-single-matcher
                   #'orig.evidence-ids
                   #'lower-multi-to-single-committer
                   #'lower-multi-to-single-binder
                   #'orig)]))

(define-syntax (lower-multi-to-single-oncer stx)
  (syntax-parse stx
    [(_ orig::binding-info)
     #'(orig.oncer-id orig.data)]))

(define-syntax (lower-multi-to-single-matcher stx)
  (syntax-parse stx
    [(_ arg-id orig::binding-info IF success failure)
     #'(orig.matcher-id (arg-id) orig.data IF success failure)]))

(define-syntax (lower-multi-to-single-committer stx)
  (syntax-parse stx
    [(_ arg-id evidence-ids orig::binding-info)
     #'(orig.committer-id (arg-id) evidence-ids orig.data)]))

(define-syntax (lower-multi-to-single-binder stx)
  (syntax-parse stx
    [(_ arg-id evidence-ids orig::binding-info)
     #'(orig.binder-id (arg-id) evidence-ids orig.data)]))
