#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         syntax/parse/pre
         "syntax-local.rkt"
         "private/transform.rkt"
         "hier-name-parse.rkt"
         "name-root.rkt"
         (submod "name-root.rkt" for-parse)
         "private/name-path-op.rkt"
         "private/check.rkt"
         "implicit.rkt")

;; Degenerate variant of enforestation with only prefix operators
;; that must consume the full group

(provide transformer?
         transformer-proc
         transformer

         transform-in
         transform-out
         call-as-transformer

         define-transform)

(struct transformer (proc))

(define-syntax (define-transform stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:syntax-class form)
                         #:defaults ([form #':form]))
              (~optional (~seq #:predicate form?)
                         #:defaults ([form? #'#f]))
              (~optional (~seq #:desc form-kind-str)
                         #:defaults ([form-kind-str "form"]))
              (~optional (~seq #:in-space in-space)
                         #:defaults ([in-space #'values]))
              (~optional (~seq #:name-path-op name-path-op)
                         #:defaults ([name-path-op #'name-path-op]))
              (~optional (~seq #:in-name-root-space in-name-root-space)
                         #:defaults ([in-name-root-space #'values]))
              (~optional (~seq #:name-root-ref name-root-ref)
                         #:defaults ([name-root-ref #'name-root-ref]))
              (~optional (~seq #:transformer-ref transformer-ref)
                         #:defaults ([transformer-ref #'transformer-ref]))
              (~optional (~seq #:check-result check-result)
                         #:defaults ([check-result #'check-is-syntax])))
        ...)
     #`(begin
         (define-syntax-class form
           #:opaque
           #:description form-kind-str
           (pattern ((~datum group) . (~var hname (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)))
                    #:cut
                    #:do [(define head-id #'hname.name)]
                    #:do [(define t (syntax-local-value* (in-space head-id) transformer-ref))]
                    #:when t
                    #:attr parsed (apply-transformer t head-id
                                                     (datum->syntax #f (cons head-id #'hname.tail))
                                                     check-result))
           (pattern ((~datum group) ((~datum parsed) . _) . _)
                    #:cut
                    #:when #f
                    #:attr parsed #'#f)
           (pattern ((~datum group) head . tail)
                    #:do [(define-values (implicit-name ctx) (select-prefix-implicit #'head))]
                    #:do [(define implicit-id (datum->syntax ctx implicit-name))]
                    #:do [(define t (syntax-local-value* (in-space implicit-id) transformer-ref))]
                    #:when t
                    #:attr parsed (apply-transformer t implicit-id
                                                     (datum->syntax #f (list* implicit-id #'head #'tail))
                                                     check-result)))

         #,@(if (syntax-e #'form?)
                #`((define (form? e)
                     (syntax-parse e
                       [((~datum group) . (~var hname (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)))
                        (and (syntax-local-value* (in-space #'hname.name) transformer-ref)
                             #t)]
                       [((~datum group) ((~datum parsed) . _) . _) #f]
                       [((~datum group) head . _)
                        (define-values (implicit-name ctx) (select-prefix-implicit #'head))
                        (define implicit-id (datum->syntax ctx implicit-name))
                        (and (syntax-local-value* (in-space implicit-id) transformer-ref)
                             #t)])))
                '()))]))

(define (apply-transformer t id stx checker)
  (define proc (transformer-proc t))
  (call-as-transformer
   id
   (lambda (in out)
     (define forms (checker (proc (in stx)) proc))
     (datum->syntax #f (out forms)))))
