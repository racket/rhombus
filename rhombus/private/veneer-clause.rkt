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

(provide define-veneer-clause-syntax)

(module+ for-class
  (provide (for-syntax in-veneer-clause-space
                       veneer-clause-quote)))

(begin-for-syntax
  (provide (property-out veneer-clause-transformer)
           :veneer-clause
           :veneer-clause-form)

  (property veneer-clause-transformer transformer)

  (define-syntax-class :veneer-clause-form
    (pattern [parsed ...]))

  (define (check-veneer-clause-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::veneer-clause-form form]
      [_ (raise-bad-macro-result (proc-name proc) "`veneer` clause" form)]))

  (define in-veneer-clause-space (make-interned-syntax-introducer/add 'rhombus/veneer_clause))
  (define-syntax (veneer-clause-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/veneer_clause) #'id))]))

  (define (make-veneer-clause-transformer-ref veneer-data)
    ;; "accessor" closes over `veneer-data`:
    (lambda (v)
      (define cc (veneer-clause-transformer-ref v))
      (and cc
           (transformer (lambda (stx)
                          ((transformer-proc cc) stx veneer-data))))))

  (define-rhombus-transform
    #:syntax-class (:veneer-clause veneer-data)
    #:desc "veneer clause"
    #:parsed-tag #:rhombus/veneer_clause
    #:in-space in-veneer-clause-space
    #:transformer-ref (make-veneer-clause-transformer-ref veneer-data)
    #:check-result check-veneer-clause-result
    #:track-origin track-parsed-sequence-origin))

(define-syntax (define-veneer-clause-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-veneer-clause-space #'id)
         rhs)]))
