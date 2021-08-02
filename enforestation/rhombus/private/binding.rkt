#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     enforest/syntax-local))

(begin-for-syntax
  (provide (property-out binding-prefix-operator)
           (property-out binding-infix-operator)

           binding-transformer

           make-identifier-binding
                     
           :binding-form
           binding-form
           check-binding-result
           
           in-binding-space
           :non-binding-identifier))

(provide define-binding-syntax
         raise-binding-failure

         ;; used by "contract.rkt":
         identifier-succeed
         identifier-bind)

(begin-for-syntax
  ;; To unpack a binding transformer result:
  (define-syntax-class :binding-form
    #:datum-literals (parens group)
    (pattern (arg-id:identifier
              matcher-id:identifier
              binder-id:identifier
              data)))

  (property binding-prefix-operator prefix-operator)
  (property binding-infix-operator infix-operator)

  (define (binding-transformer name proc)
    (binding-prefix-operator name '((default . stronger)) 'macro proc))

  ;; puts pieces together into a `:binding-form`
  (define (binding-form arg-id matcher-id binder-id data)
    (datum->syntax #f (list arg-id
                            matcher-id
                            binder-id
                            data)))

  (define (make-identifier-binding id)
    (binding-form id
                  #'identifier-succeed
                  #'identifier-bind
                  id))

  (define (check-binding-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::binding-form form]
      [_ (raise-result-error (proc-name proc) "binding-result?" form)]))

  (define in-binding-space (make-interned-syntax-introducer 'rhombus/binding))

  (define-syntax-class :non-binding-identifier
    (pattern id:identifier
             #:when (not (syntax-local-value* (in-binding-space #'id) binding-prefix-operator?)))))

(define-syntax (identifier-succeed stx)
  (syntax-parse stx
    [(_ arg-id bind-id IF success fail)
     #'(IF #t success fail)]))

(define-syntax (identifier-bind stx)
  (syntax-parse stx
    [(_ arg-id bind-id)
     #'(define bind-id arg-id)]))

(define-syntax (define-binding-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-binding-space #'name) rhs))]))

(define (raise-binding-failure who what val binding)
  (error who
         (string-append "~a does not match binding pattern\n"
                        "  argument: ~v\n"
                        "  binding: ~s")
         what
         val
         binding))
