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

  (define (check-entry-point-result form proc)
    (unless (syntax? form)
      (raise-bad-macro-result (proc-name proc) "entry point" form))
    form)

  (define (check-entry-point-arity-result form proc)
    (define (bad)
      (raise-bad-macro-result (proc-name proc) #:syntax-for? #f "entry point arity" form))
    (let loop ([form form])
      (cond
        [(not form) (datum->syntax #f form)]
        [(exact-integer? form) (datum->syntax #f form)]
        [(and (list? form)
              (= 3 (length form))
              (exact-integer? (car form)))
         (define allows (cadr form))
         (define reqs (caddr form))
         (cond
           [(and (list? allows)
                 (andmap keyword? allows)
                 (or (not reqs)
                     (and (list? reqs)
                          (andmap keyword? reqs))))
            (datum->syntax #f form)]
           [(and (listable? allows)
                 (listable? reqs))
            (loop (list (car form) (to-list #f allows) (to-list #f reqs)))]
           [else (bad)])]
        [(and (listable? form) (not (list? form))) (loop (to-list #f form))]
        [else (bad)])))

  (define in-entry-point-space (make-interned-syntax-introducer/add 'rhombus/entry_point))
  
  (define-rhombus-transform
    #:syntax-class (:entry-point adjustments)
    #:desc "entry-point form"
    #:parsed-tag #:rhombus/entry_point
    #:in-space in-entry-point-space
    #:transformer-ref (lambda (v)
                        (define t (entry-point-transformer-ref v))
                        (and t (transformer
                                (lambda (stx)
                                  ((transformer-proc t) stx adjustments)))))
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
