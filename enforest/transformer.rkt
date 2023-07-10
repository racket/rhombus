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

         track-sequence-origin

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
                         #:defaults ([track-origin #'syntax-track-origin]))
              (~optional (~seq #:accept-parsed? accept-parsed?)
                         #:defaults ([accept-parsed? #'#f])))
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
                    #:attr parsed (transform-in ; back to an enclosing transformer, if any
                                   (apply-transformer t (transform-out head-id)
                                                      (begin
                                                        (transform-out ; from an enclosing transformer
                                                         (datum->syntax #f (cons #'hname.name #'hname.tail))))
                                                      track-origin
                                                      check-result)))
           #,(if (syntax-e #'accept-parsed?)
                 #`(pattern ((~datum group) (~and head ((~datum parsed) tag inside . inside-tail)) . tail)
                            #:cut
                            #:do [(unless (eq? (syntax-e #'tag) 'parsed-tag)
                                    (parsed-wrong-context-error form-kind-str #'head))]
                            #:with () #'inside-tail
                            #:with () #'tail
                            #:attr parsed #'inside)
                 #`(pattern ((~datum group) (~and head ((~datum parsed) _ . _)) . _)
                            #:cut
                            #:when #f
                            #:attr parsed #'#f))
           (pattern ((~datum group) head . tail)
                    #:do [(define-values (implicit-name* ctx) (select-prefix-implicit #'head))
                          (define implicit-name (datum->syntax ctx implicit-name*))
                          (define implicit-id (in-space implicit-name))
                          (define t (syntax-local-value* implicit-id transformer-ref))]
                    #:when t
                    #:attr parsed (transform-in
                                   (apply-transformer t (transform-out implicit-id)
                                                      (transform-out
                                                       (datum->syntax #f (list* implicit-name #'head #'tail)))
                                                      track-origin
                                                      check-result))))

         #,@(if (syntax-e #'form?)
                #`((define (form? e)
                     (syntax-parse e
                       [((~datum group) . (~var hname (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)))
                        (and (syntax-local-value* (in-space #'hname.name) transformer-ref)
                             #t)]
                       [((~datum group) ((~datum parsed) tag . _) . _) (and accept-parsed?
                                                                            (eq? (syntax-e #'tag) 'parsed-tag))]
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
   track-origin
   (lambda (in out)
     (define forms (checker (proc (in stx)) proc))
     (datum->syntax #f (out forms)))))
