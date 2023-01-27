#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol
                     "introducer.rkt")
         "definition.rkt"
         "dotted-sequence-parse.rkt"
         "forwarding-sequence.rkt"
         "parse.rkt"
         "name-root.rkt"
         "name-root-ref.rkt")

(provide namespace)

(module+ for-exports
  (provide (for-syntax parse-exports
                       exports->names)))

(define-syntax namespace
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id name-seq::dotted-identifier-sequence)
        #:with name::dotted-identifier #'name-seq
        #`((rhombus-nested-forwarding-sequence
            (define-name-root-for-exports name.name name.extends)))]
       [(form-id name-seq::dotted-identifier-sequence
                 ((~and tag block) form ...))
        #:with name::dotted-identifier #'name-seq
        #`((rhombus-nested-forwarding-sequence
            (define-name-root-for-exports name.name name.extends)
            #,(syntax-local-introduce
               #`(rhombus-nested form ...))))]))))

(define-syntax (define-name-root-for-exports stx)
  (syntax-parse stx
    [(_ name extends ex ...)
     #:with fields (parse-exports #'(combine-out ex ...))
     #'(define-name-root name
         #:extends extends
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
              (define base-ht (hash-set ht ext-sym int-id))
              ;; look for extensions (in all spaces)
              (define prefix (format "~a." (symbol->string (syntax-e int-id))))
              (for*/fold ([ht base-ht]) ([space-sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                                         #:do [(define intro (if space-sym
                                                                 (make-interned-syntax-introducer/add space-sym)
                                                                 (lambda (x) x)))
                                               (define name-root-id  (extensible-name-root (list int-id)))]
                                         #:when name-root-id
                                         [sym (in-list (syntax-bound-symbols (intro int-id)))])
                (define str (symbol->immutable-string sym))
                (cond
                  [(and (> (string-length str) (string-length prefix))
                        (string=? prefix (substring str 0 (string-length prefix))))
                   (define id (intro (datum->syntax int-id sym int-id)))
                   (cond
                     [(identifier-extension-binding? id name-root-id)
                      (define old (hash-ref ht sym #f))
                      (when (and old
                                 (not (free-identifier=? (intro old) id)))
                        (raise-syntax-error #f
                                            "duplicate export name with different bindings"
                                            id))
                      (hash-set ht sym id)]
                     [else ht])]
                  [else ht]))]))]
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

(define-for-syntax (exports->names exports-stx)
  (for/hasheq ([ex (in-list (if (syntax? exports-stx) (syntax->list exports-stx) exports-stx))])
    (define id (syntax-parse ex
                 [(id ext-id) #'ext-id]
                 [_ ex]))
    (values (syntax-e id) id)))
