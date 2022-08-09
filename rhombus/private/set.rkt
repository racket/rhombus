#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         (submod "annotation.rkt" for-class)
         "name-root.rkt"
         "static-info.rkt"
         "map-ref-set-key.rkt"
         "call-result-key.rkt"
         "parse.rkt"
         "literal.rkt")

(provide Set
         (for-space rhombus/annotation Set)
         (for-space rhombus/static-info Set))

(module+ for-ref
  (provide set?
           set-ht
           set))

(module+ for-info
  (provide (for-syntax set-static-info)
           plain-Set))

(struct set (ht)
  #:property prop:equal+hash
  (list (lambda (self other eql? mode)
          (eql? (set-ht self) (set-ht other)))
        (lambda (self hash-code mode)
          (hash-code (set-ht self)))))

(define (set-member? s v)
  (hash-ref (set-ht s) v #f))

(define (set-member! s v in?)
  (if in?
      (hash-set! (set-ht s) v #t)
      (hash-remove! (set-ht s) v)))

(define plain-Set
  (let ([Set (lambda vals
               (define base-ht (hashalw))
               (set (for/fold ([ht base-ht]) ([val (in-list vals)])
                      (hash-set ht val #t))))])
    Set))

(define-syntax empty-set
  (make-expression+binding-prefix-operator
   #'empty-set
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values #'(set #hashalw()) #'tail)]))
   ;; binding
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'empty-set-infoer #'()) #'tail)]))))

(define-syntax (empty-set-infoer stx)
  (syntax-parse stx
    [(_ static-infos datum)
     (binding-info "Set.empty"
                   #'empty-set
                   #'static-infos
                   #'()
                   #'empty-set-matcher
                   #'literal-bind-nothing
                   #'datum)]))

(define-syntax (empty-set-matcher stx)
  (syntax-parse stx
    [(_ arg-id datum IF success fail)
     #'(IF (and (set? arg-id) (eqv? 0 (hash-count (set-ht arg-id))))
           success
           fail)]))

(define-name-root Set
  #:fields
  ([empty empty-set]
   [make make-set])
  #:root
  (expression-transformer
   #'Set
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail) (values #'plain-Set #'tail)]))))

(define-for-syntax set-static-info
  #'((#%map-ref set-member?)
     (#%map-set! set-member!)
     (#%map-append set-append)))

(define-annotation-constructor Set
  ()
  #'set? set-static-info
  1
  (lambda (arg-id predicate-stxs)
    #`(for/and ([v (in-hash-keys (set-ht #,arg-id))])
        (#,(car predicate-stxs) v)))
  (lambda (static-infoss)
    #`()))

(define-static-info-syntax Set
  (#%call-result ((#%map-ref set-ref))))

(define (make-set . vals)
  (define ht (make-hashalw))
  (for ([v (in-list vals)])
    (hash-set! ht v #t))
  (set ht))

(define-static-info-syntax make-set
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
       #:literals (plain-Set)
       [(plain-Set v)
        #'(set (hash-set (set-ht set1) v #t))]
       [_
        #'(set-append/proc set1 set2)])]))

(define (set-append/proc set1 set2)
  (set (for/fold ([ht (set-ht set1)]) ([k (in-hash-keys (set-ht set2))])
         (hash-set ht k #t))))
