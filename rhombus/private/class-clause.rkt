#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt"
                     "track-parsed.rkt"
                     (for-syntax racket/base))
         "enforest.rkt")

(provide define-class-clause-syntax)

(module+ for-class
  (provide (for-syntax in-class-clause-space
                       class-clause-quote)))

(begin-for-syntax
  (provide (property-out class-clause-transformer)
           :class-clause
           :class-clause-form)

  (property class-clause-transformer transformer)

  (define-syntax-class :class-clause-form
    (pattern [parsed ...]))

  (define (check-class-clause-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::class-clause-form form]
      [_ (raise-bad-macro-result (proc-name proc) "`class` clause" form)]))

  (define in-class-clause-space (make-interned-syntax-introducer/add 'rhombus/class_clause))
  (define-syntax (class-clause-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/class_clause) #'id))]))

  (define (make-class-clause-transformer-ref class-data)
    ;; "accessor" closes over `class-data`:
    (lambda (v)
      (define cc (class-clause-transformer-ref v))
      (and cc
           (transformer (lambda (stx)
                          ((transformer-proc cc) stx class-data))))))

  (define-rhombus-transform
    #:syntax-class (:class-clause class-data)
    #:desc "class clause"
    #:parsed-tag #:rhombus/class_clause
    #:in-space in-class-clause-space
    #:transformer-ref (make-class-clause-transformer-ref class-data)
    #:check-result check-class-clause-result
    #:track-origin track-parsed-sequence-origin))

(define-syntax (define-class-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-class-clause-space #'id)
         rhs)]))
