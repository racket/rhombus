#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse
         "syntax-local.rkt"
         "private/transform.rkt"
         "name-ref-parse.rkt"
         "private/name-path-op.rkt"
         "private/check.rkt")

;; Degenerate variant of enforestation with only prefix operators

(provide transformer?
         transformer-proc
         transformer

         transform-in
         transform-out

         define-transform)

(struct transformer (proc))

(define-syntax (define-transform stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:syntax-class form)
                         #:defaults ([form #':form]))
              (~optional (~seq #:desc form-kind-str)
                         #:defaults ([form-kind-str "transformer"]))
              (~optional (~seq #:in-space in-space)
                         #:defaults ([in-space #'values]))
              (~optional (~seq #:name-path-op name-path-op)
                         #:defaults ([name-path-op #'name-path-op]))
              (~optional (~seq #:transformer-ref transformer-ref)
                         #:defaults ([transformer-ref #'transformer-ref]))
              (~optional (~seq #:check-result check-result)
                         #:defaults ([check-result #'check-is-syntax])))
        ...)
     #'(begin
         (define-syntax-class form
           (pattern ((~datum group) . (~var ref (:name-ref-seq values name-path-op)))
                    #:do [(define head-id (transform-in #'ref.name))]
                    #:do [(define t (let ([transform-in-id transform-in])
                                      (syntax-local-value* (in-space head-id) transformer-ref)))]
                    #:when t
                    #:attr parsed (apply-transformer t head-id
                                                     (datum->syntax #f (cons head-id #'ref.tail))
                                                     check-result))))]))

(define (apply-transformer t id stx checker)
  (define proc (transformer-proc t))
  (call-as-transformer
   id
   (lambda (in out)
     (define forms (checker (proc (in stx)) proc))
     (out (datum->syntax #f forms)))))
