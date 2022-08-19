#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "introducer.rkt")
         "definition.rkt"
         "forwarding-sequence.rkt"
         "parse.rkt"
         "name-root.rkt")

(provide namespace)

(define-syntax namespace
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id name:identifier
                 ((~and tag block) form ...))
        #`((rhombus-nested-forwarding-sequence
            (define-name-root-for-exports name)
            #,(syntax-local-introduce
               #`(rhombus-nested form ...))))]))))

(define-syntax (define-name-root-for-exports stx)
  (syntax-parse stx
    [(_ name ex ...)
     #:with fields (parse-exports #'(combine-out ex ...))
     #'(define-name-root name
         #:fields
         fields)]))

(define-for-syntax (parse-exports ex)
  (define ht
    (let loop ([ex ex] [ht #hasheq()] [except-ht #hasheq()])
      (syntax-parse ex
        #:datum-literals (combine-out all-spaces-out all-from-out for-meta for-label)
        [(combine-out ex ...)
         (for/fold ([ht ht]) ([ex (in-list (syntax->list #'(ex ...)))])
           (loop ex ht except-ht))]
        [(all-spaces-out o ...)
         (for/fold ([ht ht]) ([o (in-list (syntax->list #'(o ...)))])
           (define-values (ext-id int-id)
             (syntax-parse o
               [(int-id ext-id) (values #'ext-id #'int-id)]
               [_:identifier (values o o)]))
           (define ext-sym (syntax-e ext-id))
           (cond
             [(hash-ref except-ht ext-sym #f) ht]
             [else
              (define old (hash-ref ht ext-sym #f))
              (when (and old
                         (not (free-identifier=? old int-id)))
                (raise-syntax-error #f
                                    "duplicate export name with different bindings"
                                    ext-id))
              (hash-set ht ext-sym int-id)]))]
        [(all-from-out mod-path)
         (raise-syntax-error #f
                             "module re-export not supported in a namespace context"
                             #'mod-path)]
        [((~or for-meta for-label) . _)
         (raise-syntax-error #f
                             "not allowed in a namespace context"
                             ex)]
        [_
         (raise-syntax-error #f
                             "don't know how to parse export"
                             ex)])))
  (for/list ([(key val) (in-hash ht)])
    #`[#,key #,val]))
