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
         "private/check.rkt")

;; Degenerate variant of enforestation with only prefix operators that
;; must consume the full group, but also with the opportinuty consume
;; subsequent groups

(provide sequence-transformer?
         sequence-transformer-proc
         sequence-transformer
         sequence-transformer-ref

         define-sequence-transform)

(struct sequence-transformer (proc))

(define (sequence-transformer-ref v) (and (sequence-transformer? v) v))

(define-syntax (define-sequence-transform stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:syntax-class form)
                         #:defaults ([form #':form]))
              (~optional (~seq #:apply-transformer apply-transformer)
                         #:defaults ([apply-transformer #'apply-transformer]))
              (~optional (~seq #:predicate form?)
                         #:defaults ([form? #'#f]))
              (~optional (~seq #:desc form-kind-str)
                         #:defaults ([form-kind-str "form"]))
              (~optional (~seq #:in-space in-space)
                         #:defaults ([in-space #'values]))
              (~optional (~seq #:name-path-op name-path-op)
                         #:defaults ([name-path-op #'name-path-op]))
              (~optional (~seq #:name-root-ref name-root-ref)
                         #:defaults ([name-root-ref #'name-root-ref]))
              (~optional (~seq #:in-name-root-space in-name-root-space)
                         #:defaults ([in-name-root-space #'values]))
              (~optional (~seq #:transformer-ref transformer-ref)
                         #:defaults ([transformer-ref #'sequence-transformer-ref]))
              (~optional (~seq #:check-result check-result)
                         #:defaults ([check-result #'check-is-syntax]))
              (~optional (~seq #:track-origin track-origin)
                         #:defaults ([track-origin #'syntax-track-origin])))
        ...)
     #`(begin
         (define-syntax-class form
           (pattern ((~datum group) . (~var hname (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)))
                    #:do [(define head-id #'hname.name)]
                    #:do [(define t (syntax-local-value* (in-space head-id) transformer-ref))]
                    #:when t
                    #:attr id head-id
                    #:attr tail #'hname.tail))
         (define (apply-transformer head-name head-tail tail-tail)
           (define head-id (in-space head-name))
           (define t (syntax-local-value* head-id transformer-ref))
           (define-values (parsed remaining-tail)
             (apply-sequence-transformer t (transform-out head-id)
                                         (transform-out (datum->syntax #f (cons head-name head-tail)))
                                         tail-tail
                                         track-origin
                                         check-result))
           (values (transform-in parsed) (transform-in remaining-tail)))
         #,@(if (syntax-e #'form?)
                #`((define (form? e)
                     (syntax-parse e
                       [((~datum group) . (~var hname (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)))
                        (and (syntax-local-value* (in-space #'hname.name) transformer-ref)
                             #t)]
                       [_ #f])))
                '()))]))

(define (apply-sequence-transformer t id stx tail track-origin checker)
  (define proc (sequence-transformer-proc t))
  (call-as-transformer
   id
   track-origin
   (lambda (in out)
     (define-values (forms new-tail) (proc (in stx) (in tail)))
     (check-transformer-result (out (datum->syntax #f (checker forms proc)))
                               (out new-tail)
                               proc))))
