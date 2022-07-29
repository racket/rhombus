#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "map-ref-set-key.rkt"
         "call-result-key.rkt"
         "parse.rkt")

(provide Set
         (for-space rhombus/annotation Set)
         (for-space rhombus/static-info Set)

         make_set
         (for-space rhombus/static-info make_set))

(module+ for-ref
  (provide set?
           set-ht
           set))

(module+ for-info
  (provide (for-syntax set-static-info)))

(struct set (ht))

(define (set-member? s v)
  (hash-ref (set-ht s) v #f))

(define (set-member! s v in?)
  (if in?
      (hash-set! (set-ht s) v #t)
      (hash-remove! (set-ht s) v)))

(define (Set . vals)
  (define base-ht (hashalw))
  (set (for/fold ([ht base-ht]) ([val (in-list vals)])
         (hash-set ht val #t))))

(define-for-syntax set-static-info
  #'((#%map-ref set-member?)
     (#%map-set! set-member!)
     (#%map-append set-append)))

(define-annotation-syntax Set
  (annotation-constructor #'Set #'set? set-static-info
                          1
                          (lambda (arg-id predicate-stxs)
                            #`(for/and ([v (in-hash-keys (set-ht #,arg-id))])
                                (#,(car predicate-stxs) v)))
                          (lambda (static-infoss)
                            #`())))

(define-static-info-syntax Set
  (#%call-result ((#%map-ref set-ref))))

(define (make_set . vals)
  (define ht (make-hashalw))
  (for ([v (in-list vals)])
    (hash-set! ht v #t))
  (set ht))

(define-static-info-syntax make_set
  (#%call-result ((#%map-ref set-member?)
                  (#%map-set! set-member!))))

(define (set-ref s v)
  (hash-ref (set-ht s) v #f))

;; macro to optimize to an inline functional update
(define-syntax (set-append stx)
  (syntax-parse stx
    #:literals (Set)
    [(_ set1 set2)
     (syntax-parse (unwrap-static-infos #'set2)
       #:literals (Set)
       [(Set v)
        #'(set (hash-set (set-ht set1) v #t))]
       [_
        #'(set-append/proc set1 set2)])]))

(define (set-append/proc set1 set2)
  (set (for/fold ([ht (set-ht set1)]) ([k (in-hash-keys (set-ht set2))])
         (hash-set ht k #t))))
