#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         syntax/parse/pre
         "syntax-local.rkt"
         "private/transform.rkt"
         "hier-name-parse.rkt"
         "name-root.rkt"
         "private/name-path-op.rkt"
         "private/check.rkt"
         "implicit.rkt")

;; Degenerate variant of enforestation with only prefix operators
;; that must consume the full group

(provide transformer?
         transformer-proc
         transformer
         transformer-ref

         call-as-transformer

         track-sequence-origin

         define-transform)

(struct transformer (proc))

(define (transformer-ref v) (and (transformer? v) v))

(define-syntax (define-transform stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:syntax-class form)
                         #:defaults ([form #':form]))
              (~optional (~seq #:transform transform-id)
                         #:defaults ([transform-id #'#f]))
              (~optional (~seq #:predicate form?)
                         #:defaults ([form? #'#f]))
              (~optional (~seq #:desc form-kind-str)
                         #:defaults ([form-kind-str "form"]))
              (~optional (~seq #:parsed-tag parsed-tag:keyword)
                         #:defaults ([parsed-tag #'#:transformed]))
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
                         #:defaults ([check-result #'check-is-syntax]))
              (~optional (~seq #:track-origin track-origin)
                         #:defaults ([track-origin #'syntax-track-origin])))
        ...)
     #`(begin
         (define-syntax-class form
           #:opaque
           #:description form-kind-str
           (pattern ((~datum group) . (~var hname (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)))
                    #:cut
                    #:do [(define head-id (in-space #'hname.name))
                          (define t (syntax-local-value* head-id transformer-ref))]
                    #:when t
                    #:with parsed (transform-in ; back to an enclosing transformer, if any
                                   (apply-transformer t (transform-out head-id)
                                                      (begin
                                                        (transform-out ; from an enclosing transformer
                                                         (datum->syntax #f (cons #'hname.name #'hname.tail))))
                                                      track-origin
                                                      check-result)))
           (pattern ((~datum group) (~and head ((~datum parsed) tag inside . inside-tail)) . tail)
                    #:when (eq? (syntax-e #'tag) 'parsed-tag)
                    #:cut
                    #:with () #'inside-tail
                    #:with () #'tail
                    #:with parsed #'inside)
           (pattern ((~datum group) head . tail)
                    #:do [(define-values (implicit-name* ctx) (select-prefix-implicit #'head))
                          (define implicit-name (datum->syntax ctx implicit-name*))
                          (define implicit-id (in-space implicit-name))
                          (define t (syntax-local-value* implicit-id transformer-ref))]
                    #:when t
                    #:with parsed (transform-in
                                   (apply-transformer t (transform-out implicit-id)
                                                      (transform-out
                                                       (datum->syntax #f (list* implicit-name #'head #'tail)))
                                                      track-origin
                                                      check-result))))

         #,@(if (syntax-e #'transform-id)
                #`((define (transform-id e)
                     (syntax-parse #`(group . #,e)
                       [(~var p form) #'p.parsed])))
                #`())

         #,@(if (syntax-e #'form?)
                #`((define (form? e)
                     (syntax-parse e
                       [((~datum group) . (~var hname (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)))
                        (and (syntax-local-value* (in-space #'hname.name) transformer-ref)
                             #t)]
                       [((~datum group) ((~datum parsed) tag . _) . _) (eq? (syntax-e #'tag) 'parsed-tag)]
                       [((~datum group) head . _)
                        (define-values (implicit-name* ctx) (select-prefix-implicit #'head))
                        (define implicit-name (datum->syntax ctx implicit-name*))
                        (and (syntax-local-value* (in-space implicit-name) transformer-ref)
                             #t)])))
                '()))]))

(define (apply-transformer t id stx track-origin checker)
  (define proc (transformer-proc t))
  (call-as-transformer
   id
   (list stx)
   track-origin
   (lambda (stx)
     (define forms (checker (proc stx) proc))
     (datum->syntax #f forms))))
