#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol
                     syntax/datum
                     "introducer.rkt"
                     "id-binding.rkt")
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
       [(form-id #:open
                 ((~and tag block) form ...))
        (define intro (make-syntax-introducer))
        #`((rhombus-nested-forwarding-sequence
            (open-exports plain #,(intro #'scoped))
            #,(intro
               #`(rhombus-nested form ...))))]
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
    [(_ name extends ctx all-ctx ex ...)
     #:with fields (parse-exports #'(combine-out ex ...))
     #'(define-name-root name
         #:extends extends
         #:fields
         fields)]))

(define-syntax (open-exports stx)
  (syntax-parse stx
    [(_ plain scoped ctx all-ctx ex ...)
     #:with fields (parse-exports #'(combine-out ex ...))
     (define delta (let ([d1 (make-syntax-delta-introducer #'scoped #'plain)]
                         [d2 (make-syntax-delta-introducer #'all-ctx #'ctx)])
                     (lambda (id mode)
                       (d2 (d1 id mode) mode))))
     (define (expose id) (delta id 'remove))
     (define (generate-definitions ext-id int-id rule)
       (define defs
         (for/list ([space-sym (in-list (syntax-parse rule
                                          #:datum-literals (only except)
                                          [()  (cons #f (syntax-local-module-interned-scope-symbols))]
                                          [(only space ...)
                                           (datum (space ...))]
                                          [(except space ...)
                                           (define spaces (for/hasheq ([sym (in-list (datum (space ...)))])
                                                            (values sym #t)))
                                           (for/list ([sp (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                                                      #:when (hash-ref spaces sp #f))
                                             sp)]
                                          [_ (error "bad rule")]))]
                    #:do [(define intro (if space-sym
                                            (make-interned-syntax-introducer space-sym)
                                            (lambda (x mode) x)))
                          (define space-int-id (intro int-id 'add))]
                    #:when (if space-sym
                               (identifier-distinct-binding* space-int-id int-id)
                               (identifier-binding* space-int-id)))
           #`(define-syntax #,(expose (intro ext-id 'add)) (make-rename-transformer (quote-syntax #,space-int-id)))))
       defs)
     #`(begin
         #,@(apply
             append
             (for/list ([field (in-list (syntax->list #'fields))])
               (syntax-parse field
                 [id:identifier
                  (generate-definitions #'id #'id #'())]
                 [(ext-id int-id . rule)
                  (generate-definitions #'ext-id #'int-id #'rule)]
                 [_
                  (raise-syntax-error 'namespace+open "unsupported" field)]))))]))

(define-for-syntax (parse-exports ex)
  (define ht
    (let loop ([ex ex] [ht #hasheq()] [except-ht #hasheq()] [spaces #f] [spaces-mode #f])
      (syntax-parse ex
        #:datum-literals (combine-out all-spaces-out all-from-out for-meta for-label
                                      only-spaces-out except-spaces-out)
        [(combine-out ex ...)
         (for/fold ([ht ht]) ([ex (in-list (syntax->list #'(ex ...)))])
           (loop ex ht except-ht spaces spaces-mode))]
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
                         (not (free-identifier=? (ext+int+rule-int old) int-id)))
                (raise-syntax-error #f
                                    "duplicate export name with different bindings"
                                    ext-id))
              (define base-ht (hash-set ht ext-sym (make-ext+int+rule ext-id int-id spaces-mode spaces)))
              ;; look for extensions (in all spaces)
              (define prefix (format "~a." (symbol->string (syntax-e int-id))))
              (for*/fold ([ht base-ht]) ([space-sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                                         #:when (cond
                                                  [(eq? spaces-mode 'only) (hash-ref spaces space-sym #f)]
                                                  [(eq? spaces-mode 'except) (not (hash-ref spaces space-sym #f))]
                                                  [else #t])
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
                                 (not (free-identifier=? (intro (ext+int+rule-int old)) id)))
                        (raise-syntax-error #f
                                            "duplicate export name with different bindings"
                                            id))
                      (define ext-ext-id (datum->syntax ext-id sym))
                      (hash-set ht sym (make-ext+int+rule ext-ext-id id spaces-mode spaces))]
                     [else ht])]
                  [else ht]))]))]
        [(only-spaces-out ex space ...)
         (define new-spaces
           (for/hasheq ([sp (in-list (syntax->list #'(space ...)))])
             (values (syntax-e sp) sp)))
         (cond
           [(not spaces-mode)
            (loop #'ex ht except-ht new-spaces 'only)]
           [else
            (for ([sp (in-hash-keys spaces)])
              (unless (hash-ref new-spaces sp #f)
                (raise-syntax-error #f
                                    "space not included in nested modification"
                                    (hash-ref spaces sp))))
            (cond
              [(eq? spaces-mode 'only)
               (loop #'ex ht except-ht spaces 'only)]
              [else
               (define keep-spaces (for/fold ([new-spaces new-spaces]) ([sp (in-hash-keys spaces)])
                                     (hash-remove new-spaces sp)))
               (loop #'ex ht except-ht keep-spaces 'only)])])]
        [(except-spaces-out ex space ...)
         (define new-spaces
           (for/hasheq ([sp (in-list (syntax->list #'(space ...)))])
             (values (syntax-e sp) sp)))
         (cond
           [(not spaces-mode)
            (loop #'ex ht except-ht new-spaces 'except)]
           [else
            (for ([sp (in-hash-keys spaces)])
              (when (hash-ref new-spaces sp #f)
                (raise-syntax-error #f
                                    "space excluded in nested modification"
                                    (hash-ref spaces sp))))
            (cond
              [(eq? spaces-mode 'only)
               (loop #'ex ht except-ht spaces 'only)]
              [else
               (define remove-spaces (for/fold ([new-spaces new-spaces]) ([(sp id) (in-hash spaces)])
                                       (hash-set new-spaces sp id)))
               (loop #'ex ht except-ht remove-spaces 'except)])])]
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
  (for/list ([(key ext+int+rule) (in-hash ht)])
    #`[#,(car ext+int+rule) #,@(cdr ext+int+rule)]))

(define-for-syntax (exports->names exports-stx)
  (for/hasheq ([ex (in-list (if (syntax? exports-stx) (syntax->list exports-stx) exports-stx))])
    (define id (syntax-parse ex
                 [(id ext-id) #'ext-id]
                 [_ ex]))
    (values (syntax-e id) id)))


(define-for-syntax (make-ext+int+rule ext-id int-id spaces-mode spaces)
  (if spaces-mode
      (list* ext-id int-id spaces-mode (hash-keys spaces #t))
      (list ext-id int-id)))

(define-for-syntax (ext+int+rule-int ext+int+rule)
  (cadr ext+int+rule))
