#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt"
                     "to-list.rkt")
         "enforest.rkt")

(module+ for-class
  (provide (for-syntax in-entry-point-space)))

(provide define-entry-point-syntax)

(begin-for-syntax
  (provide (property-out entry-point-transformer)
           :entry-point
           :entry-point-arity
           check-entry-point-arity-result)

  (property entry-point-transformer transformer (arity-extract))

  (define (check-entry-point-result form proc adjustments)
    (unless (syntax? form)
      (raise-bad-macro-result (proc-name proc) "entry point" form))
    form)

  (define (check-entry-point-arity-result form-in proc)
    (define (bad)
      (raise-bad-macro-result (proc-name proc)
                              #:syntax-for? #f
                              "entry point arity" form-in))
    (define (arity-list? v)
      (and (pair? v)
           (pair? (cdr v))
           (pair? (cddr v))
           (null? (cdddr v))))
    (define (check-keyword-list v)
      (unless (and (list? v)
                   (andmap keyword? v))
        (bad))
      v)
    (define form
      (cond
        [(or (not form-in)
             (exact-integer? form-in))
         form-in]
        [else
         (define form (to-list #f form-in))
         (unless (arity-list? form) (bad))
         (define-values (mask required-kws-in allowed-kws-in)
           (values (car form) (cadr form) (caddr form)))
         (unless (exact-integer? mask) (bad))
         (define required-kws
           (check-keyword-list (to-list #f required-kws-in)))
         (define allowed-kws
           (cond
             [(not allowed-kws-in) allowed-kws-in]
             [else (check-keyword-list (to-list #f allowed-kws-in))]))
         (list mask required-kws allowed-kws)]))
    (datum->syntax #f form))

  (define in-entry-point-space (make-interned-syntax-introducer/add 'rhombus/entry_point))

  (define-rhombus-transform
    #:syntax-class (:entry-point adjustments)
    #:desc "entry-point form"
    #:parsed-tag #:rhombus/entry_point
    #:in-space in-entry-point-space
    #:transformer-ref entry-point-transformer-ref
    #:check-result check-entry-point-result)

  (define-rhombus-transform
    #:syntax-class :entry-point-arity
    #:desc "entry-point form"
    #:parsed-tag #:rhombus/entry_point
    #:in-space in-entry-point-space
    #:transformer-ref (lambda (v)
                        (define t (entry-point-transformer-ref v))
                        (and t (transformer (entry-point-transformer-arity-extract t))))
    #:check-result check-entry-point-arity-result))

(define-syntax (define-entry-point-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-entry-point-space #'name) rhs))]))
