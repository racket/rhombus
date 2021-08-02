#lang racket/base
(require syntax/parse
         "syntax-local.rkt"
         "private/transform.rkt")

;; Degenerate variant of enforestation with only prefix "operators"
;; with identifier names

(provide transformer?
         transformer-proc
         transformer

         define-transform-class)

(struct transformer (proc))

(define-syntax-rule (define-transform-class
                      :form
                      form-kind-str
                      transformer-ref
                      check-result)
  (begin
    (define-syntax-class :form
      (pattern ((~datum group) head:identifier . tail)
               #:do [(define head-id (transform-in #'head))]
               #:do [(define t (syntax-local-value* head-id transformer-ref))]
               #:when t
               #:attr parsed (transform-out
                              (apply-transformer (transformer-ref t) head-id
                                                 (transform-in (datum->syntax #f (cons head-id #'tail)))
                                                 check-result))))))

(define (apply-transformer t id stx checker)
  (define proc (transformer-proc t))
  (call-as-transformer
   id
   (lambda (in out)
     (define forms (checker (proc (in stx)) proc))
     (out (datum->syntax #f forms)))))
