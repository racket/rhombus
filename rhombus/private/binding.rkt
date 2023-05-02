#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/operator
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "introducer.rkt"
                     "annotation-string.rkt"
                     "name-path-op.rkt"
                     (for-syntax racket/base))
         "static-info.rkt"
         "realm.rkt"
         "error.rkt"
         "name-root-ref.rkt"
         "dotted-sequence-parse.rkt")

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

           binding-extension-combine))

(provide define-binding-syntax
         raise-binding-failure
         always-succeed)

(begin-for-syntax
  ;; To unpack a binding transformer result:
  (define-syntax-class :binding-form
    #:datum-literals (parens group)
    (pattern (infoer-id:identifier
              data)))

  ;; To call an infoer:
  (define-syntax-class :binding-impl
    #:datum-literals (parens group)
    (pattern (~and form (infoer-id . _))
             #:do [(define proc (syntax-local-value* #'infoer-id (lambda (v)
                                                                   (and (procedure? v)
                                                                        v))))
                   (unless proc
                     (raise-syntax-error #f
                                         "cannot find a transformer for an infoer"
                                         #'infoer-id))]
             #:with info (check-binding-info-result
                          (transform-out
                           (let ([form (transform-in #'form)])
                             (call-as-transformer
                              #'infoer-id
                              (lambda (in out)
                                (out (proc (in form)))))))
                          proc)))

  ;; To unpack a binding infoer result:
  (define-syntax-class :binding-info
    #:datum-literals (parens group)
    (pattern (annotation-str:string
              name-id:identifier
              (~and static-infos ((:identifier _) ...))
              (~and bind-infos ((bind-id:identifier (~and bind-uses (bind-use ...)) (~and bind-static-info (:identifier _)) ...) ...))
              matcher-id:identifier
              committer-id:identifier
              binder-id:identifier
              data)))

  (property binding-prefix-operator prefix-operator)
  (property binding-infix-operator infix-operator)

  (define (binding-transformer proc)
    (binding-prefix-operator (quote-syntax unused) '((default . stronger)) 'macro proc))

  ;; puts pieces together into a `:binding-form`
  (define (binding-form infoer-id data)
    (datum->syntax #f (list infoer-id
                            data)))

  ;; puts pieces together into a `:binding-info`
  (define (binding-info annotation-str name-id static-infos bind-infos matcher-id committer-id binder-id data)
    (datum->syntax #f (list annotation-str
                            name-id
                            static-infos
                            bind-infos
                            matcher-id
                            committer-id
                            binder-id
                            data)))

  (define (make-identifier-binding id)
    (binding-form #'identifier-infoer
                  id))

  (define (check-binding-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::binding-form form]
      [_ (raise-result-error (proc-name proc) "binding-result?" form)]))

  (define (check-binding-info-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::binding-info form]
      [_ (raise-result-error (proc-name proc) "binding-info-result?" form)]))

  (define in-binding-space (make-interned-syntax-introducer/add 'rhombus/bind))
  (define-syntax (bind-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/bind) #'id))]))

  (define extension-syntax-property-key (gensym 'extension))
  (define (binding-extension-combine id prefix)
    (syntax-property id extension-syntax-property-key prefix)))

(define-syntax (identifier-infoer stx)
  (syntax-parse stx
    [(_ static-infos id)
     (binding-info annotation-any-string
                   #'id
                   #'static-infos
                   #'((id (0) . static-infos))
                   #'always-succeed
                   #'identifier-commit
                   #'identifier-bind
                   (let ([prefix (syntax-property #'id extension-syntax-property-key)])
                     (if prefix
                         #`[id #,prefix]
                         #'id)))]))

(define-syntax (always-succeed stx)
  (syntax-parse stx
    [(_ _ _ IF success fail)
     #'(IF #t success fail)]))

(define-syntax (identifier-commit stx)
  (syntax-parse stx
    [(_ arg-id bind-id*)
     #'(begin)]))

(define-syntax (identifier-bind stx)
  (syntax-parse stx
    [(_ arg-id [bind-id prefix-id])
     (define l (build-definitions/maybe-extension #f #'bind-id #'prefix-id
                                                  #'arg-id))
     (if (= 1 (length l))
         (car l)
         #`(begin #,@l))]
    [(_ arg-id bind-id)
     #'(define bind-id arg-id)]))

(define-syntax (define-binding-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-binding-space #'name) rhs))]))

(define (raise-binding-failure who what val annotation-str)
  (raise-contract-error
   who
   (string-append "~a does not satisfy annotation\n"
                  "  ~a: ~v\n"
                  "  annotation: ~a")
   what
   what
   val
   (error-contract->adjusted-string
    annotation-str
    rhombus-realm)))
