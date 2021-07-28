#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "op.rkt"
                     "transformer.rkt"
                     "property.rkt"
                     "check.rkt"
                     "syntax-local.rkt")
         "property-out.rkt")

(provide (for-syntax (property-out binding-prefix-operator)
                     (property-out binding-infix-operator)
                     
                     (property-out binding-transformer)

                     make-identifier-binding

                     :binding-form
                     binding-form
                     check-binding-result

                     in-binding-space
                     :non-binding-identifier)

         define-binding-syntax
         raise-binding-failure)

(begin-for-syntax
  ;; To unpack a binding transformer result:
  (define-syntax-class :binding-form
    #:datum-literals (parens group)
    (pattern ((~and var-ids (var-id:identifier ...))
              check-proc-expr
              post-defn)))

  (property binding-prefix-operator prefix-operator)
  (property binding-infix-operator infix-operator)

  (property binding-transformer transformer)

  ;; puts pieces together into a `:binding-form`
  (define (binding-form ids check-proc-expr post-defn)
    (datum->syntax #f (list ids
                            check-proc-expr
                            post-defn)))

  (define (make-identifier-binding id)
    (binding-form (list id)
                  #'(lambda (v) (values #t v))
                  #'(begin)))

  (define (check-binding-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::binding-form form]
      [_ (raise-result-error (proc-name proc) "binding-result?" form)]))

  (define in-binding-space (make-interned-syntax-introducer 'rhombus/binding))

  (define-syntax-class :non-binding-identifier
    (pattern id:identifier
             #:when (not (syntax-local-value* (in-binding-space #'id) binding-transformer?)))))

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
