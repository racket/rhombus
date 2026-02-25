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
                     "macro-result.rkt")
         "realm.rkt"
         "dotted-sequence-parse.rkt"
         "realm.rkt"
         "binding-failure.rkt")

(begin-for-syntax
  (provide (property-out binding-prefix-operator)
           (property-out binding-infix-operator)

           binding-transformer

           make-identifier-binding

           :binding-form
           binding-form
           check-binding-result

           :binding-impl

           :binding-info
           binding-info

           in-binding-space
           bind-quote

           binding-extension-combine
           disallow-binding-as-namespace-extension

           (struct-out binding-prefix+infix-operator)))

(provide define-binding-syntax
         raise-binding-failure
         always-succeed
         empty-oncer)

(begin-for-syntax
  ;; To unpack a binding transformer result:
  (define-syntax-class :binding-form
    (pattern (infoer-id:identifier
              data)))

  ;; To call an infoer:
  (define-syntax-class :binding-impl
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
                          proc)))

  (define-syntax-class :evidence-id-tree
    (pattern _:identifier)
    (pattern (_::evidence-id-tree ...)))

  ;; To unpack a binding infoer result:
  (define-syntax-class :binding-info
    (pattern (annotation-str:string
              name-id:identifier
              (~and static-infos ((:identifier _) ...))
              (~and bind-infos ((bind-id:identifier (~and bind-uses (bind-use ...)) (~and bind-static-info (:identifier _)) ...) ...))
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

  ;; puts pieces together into a `:binding-form`
  (define (binding-form infoer-id data)
    (datum->syntax #f (list infoer-id
                            data)))

  ;; puts pieces together into a `:binding-info`
  (define binding-info
    (case-lambda
      [(annotation-str name-id static-infos bind-infos oncer-id matcher-id evidence-ids committer-id binder-id data)
       (datum->syntax #f (list annotation-str
                               name-id
                               static-infos
                               bind-infos
                               oncer-id
                               matcher-id
                               evidence-ids
                               committer-id
                               binder-id
                               data))]
      [args (error 'binding-info "bad args: ~s" args)]))

  (define (make-identifier-binding id)
    (binding-form #'identifier-infoer
                  id))

  (define (check-binding-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::binding-form form]
      [_ (raise-bad-macro-result (proc-name proc) "binding" form)]))

  (define (check-binding-info-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::binding-info form]
      [_ (raise-bad-macro-result (proc-name proc) "binding-info" form)]))

  (define in-binding-space (make-interned-syntax-introducer/add 'rhombus/bind))
  (define-syntax (bind-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/bind) #'id))]))

  (define extension-syntax-property-key (gensym 'extension))
  (define (binding-extension-combine prefix field-id id)
    (syntax-property id extension-syntax-property-key prefix))

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
                               id)))])))

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
     #'(void)]))

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
