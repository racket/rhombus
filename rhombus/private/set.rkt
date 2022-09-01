#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "name-root.rkt"
         "static-info.rkt"
         "map-ref-set-key.rkt"
         "call-result-key.rkt"
         "parse.rkt"
         "literal.rkt"
         "realm.rkt"
         "setmap-parse.rkt"
         "dot-parse.rkt"
         "parens.rkt")

(provide (rename-out [Set-expr Set])
         (for-space rhombus/annotation Set)
         (for-space rhombus/static-info Set)

         (rename-out [MutableSet-expr MutableSet])
         (for-space rhombus/static-info MutableSet))

(module+ for-ref
  (provide set?
           set-ht
           set))

(module+ for-builtin
  (provide set?
           set-method-table))

(module+ for-info
  (provide (for-syntax set-static-info)
           Set-build))

(struct set (ht)
  #:property prop:equal+hash
  (list (lambda (self other eql? mode)
          (eql? (set-ht self) (set-ht other)))
        (lambda (self hash-code mode)
          (hash-code (set-ht self)))))

(define set-method-table
  (hash 'length (let ([length (lambda (s)
                                (unless (set? s)
                                  (raise-argument-error* 'Set.length rhombus-realm "Set" s))
                                (hash-count (set-ht s)))])
                  (method1 length))))

(define (set-member? s v)
  (hash-ref (set-ht s) v #f))

(define (set-member! s v in?)
  (if in?
      (hash-set! (set-ht s) v #t)
      (hash-remove! (set-ht s) v)))

(define (set-count s)
  (hash-count (set-ht s)))

(define-syntax set-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'set-count)]
        [else #f])))))

(define-syntax (Set-build stx)
  (syntax-parse stx
    [(_ elem ...)
     #`(set (hashalw (~@ elem #t) ...))]))

(define (Set . vals)
  (define base-ht (hashalw))
  (set (for/fold ([ht base-ht]) ([val (in-list vals)])
         (hash-set ht val #t))))

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

(define-name-root Set-expr
  #:fields
  ([empty empty-set]
   [length set-count])
  #:root
  (expression-transformer
   #'Set
   (lambda (stx)
     (syntax-parse stx
       [(form-id (~and content (_::braces . _)) . tail)
        (define-values (shape args) (parse-setmap-content #'content
                                                          #:shape 'set
                                                          #:who (syntax-e #'form-id)))
        (values (wrap-static-info*
                 (quasisyntax/loc stx
                   (Set-build #,@args))
                 set-static-info)
                #'tail)]
       [(_ . tail) (values #'Set #'tail)]))))

(define-for-syntax set-static-info
  #'((#%map-ref set-member?)
     (#%map-append set-append)
     (#%dot-provider set-instance)))

(define-for-syntax mutable-set-static-info
  #`((#%map-set! set-member!)
     . #,set-static-info))

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
  (#%call-result #,set-static-info))

(define (MutableSet . vals)
  (define ht (make-hashalw))
  (for ([v (in-list vals)])
    (hash-set! ht v #t))
  (set ht))

(define-syntax MutableSet-expr
  (expression-transformer
   #'Set
   (lambda (stx)
     (syntax-parse stx
       [(form-id (~and content (_::braces . _)) . tail)
        (define-values (shape args) (parse-setmap-content #'content
                                                          #:shape 'set
                                                          #:who (syntax-e #'form-id)))
        (values (wrap-static-info*
                 (quasisyntax/loc stx
                   (MutableSet #,@args))
                 mutable-set-static-info)
                #'tail)]
       [(_ . tail) (values #'MutableSet #'tail)]))))

(define-static-info-syntax MutableSet
  (#%call-result #,mutable-set-static-info))

(define (set-ref s v)
  (hash-ref (set-ht s) v #f))

;; macro to optimize to an inline functional update
(define-syntax (set-append stx)
  (syntax-parse stx
    #:literals (Set)
    [(_ set1 set2)
     (syntax-parse (unwrap-static-infos #'set2)
       #:literals (Set-build)
       [(Set-build v)
        #'(set (hash-set (set-ht set1) v #t))]
       [_
        #'(set-append/proc set1 set2)])]))

(define (set-append/proc set1 set2)
  (set (for/fold ([ht (set-ht set1)]) ([k (in-hash-keys (set-ht set2))])
         (hash-set ht k #t))))
