#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol
                     syntax/datum
                     "introducer.rkt"
                     "id-binding.rkt"
                     "expose.rkt")
         "definition.rkt"
         "dotted-sequence-parse.rkt"
         "forwarding-sequence.rkt"
         "parse.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "parens.rkt")

(provide (for-space rhombus/defn
                    namespace))

(module+ for-exports
  (provide (for-syntax parse-exports
                       exports->names)))

(define-defn-syntax namespace
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id #:open
                 (_::block form ...))
        (define intro (make-syntax-introducer))
        #`((rhombus-nested-forwarding-sequence
            (open-exports plain #,(intro #'scoped))
            #,(intro
               #`(rhombus-nested form ...))))]
       [(form-id name-seq::dotted-identifier-sequence)
        #:with name::dotted-identifier #'name-seq
        #`((rhombus-nested-forwarding-sequence
            (define-name-root-for-exports [name.name name.extends plain scoped])))]
       [(form-id name-seq::dotted-identifier-sequence
                 (_::block form ...))
        #:with name::dotted-identifier #'name-seq
        (define intro syntax-local-introduce)
        #`((rhombus-nested-forwarding-sequence
            (define-name-root-for-exports [name.name name.extends plain #,(intro #'scoped)])
            #,(intro
               #`(rhombus-nested form ...))))]))))

(define-syntax (define-name-root-for-exports stx)
  (syntax-parse stx
    [(_ [name extends base-ctx scoped-ctx]
        [#:ctx forward-base-ctx forward-ctx]
        ex ...)
     #:with fields (parse-exports #'(combine-out ex ...)
                                  (make-expose ((make-syntax-delta-introducer #'forward-ctx
                                                                              #'forward-base-ctx)
                                                #'scoped-ctx)
                                               #'base-ctx))
     #'(define-name-root name
         #:extends extends
         #:fields
         fields)]))

(define-syntax (open-exports stx)
  (syntax-parse stx
    [(_ plain scoped [#:ctx ctx all-ctx] ex ...)
     (define expose (make-expose
                     ((make-syntax-delta-introducer #'all-ctx #'ctx) #'scoped)
                     #'plain))
     (define fields (parse-exports #'(combine-out ex ...) expose))
     (define (generate-definitions ext-id int-id rule)
       (for/list ([space+id (in-list (let loop ([rule rule])
                                       (syntax-parse rule
                                         [() (for/list ([space (in-list (cons #f (syntax-local-module-interned-scope-symbols)))])
                                               (list (datum->syntax #f space) int-id))]
                                         [(#:space space+ids . rule-rest)
                                          (append
                                           (map syntax->list (syntax->list #'space+ids))
                                           (loop #'rule-rest))]
                                         [(#:only space ...)
                                          (for/list ([space (in-list (syntax->list #'(space ...)))])
                                            (list space int-id))]
                                         [(#:except space ...)
                                          (define spaces (for/hasheq ([sym (in-list (datum (space ...)))])
                                                           (values sym #t)))
                                          (for/list ([sp (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                                                     #:when (hash-ref spaces sp #f))
                                            (list sp int-id))]
                                         [_ (error "bad rule")])))]
                  #:do [(define space-sym (syntax-e (car space+id)))
                        (define int-id (cadr space+id))
                        (define intro (if space-sym
                                          (make-interned-syntax-introducer space-sym)
                                          (lambda (x mode) x)))
                        (define space-int-id (intro int-id 'add))]
                  #:when (if space-sym
                             (identifier-distinct-binding* space-int-id int-id)
                             (identifier-binding* space-int-id)))
         #`(define-syntax #,(expose (intro ext-id 'add)) (make-rename-transformer (quote-syntax #,space-int-id)))))
     #`(begin
         #,@(apply
             append
             (for/list ([field (in-list fields)])
               (syntax-parse field
                 [id:identifier
                  (generate-definitions #'id #'id #'())]
                 [(ext-id int-id . rule)
                  (generate-definitions #'ext-id #'int-id #'rule)]
                 [_
                  (raise-syntax-error 'namespace+open "unsupported" field)]))))]))

(define-for-syntax (parse-exports ex expose)
  (define (use-space? space-sym spaces-mode spaces)
    (cond
      [(eq? spaces-mode '#:only) (hash-ref spaces space-sym #f)]
      [(eq? spaces-mode '#:except) (not (hash-ref spaces space-sym #f))]
      [else #t]))
  (define ht
    ;; maps a symbol key to `next+int+rule`, which is like a namespace rule spec
    ;; (see "name-root.rkt"), but only identifiers are syntax objects
    (let loop ([ex ex] [ht #hasheq()] [except-ht #hasheq()] [spaces #f] [spaces-mode #f])
      (define (add-name-at-all-spaces ht ext-id int-id spaces spaces-mode)
        (define ext-sym (syntax-e ext-id))
        (cond
          [(hash-ref except-ht ext-sym #f) ht]
          [else
           (define base-ht (hash-set ht ext-sym
                                     (merge-ext+int+rule
                                      ext-id
                                      (hash-ref ht ext-sym #f)
                                      (make-ext+int+rule ext-id int-id spaces-mode spaces))))
           ;; look for extensions (in all spaces)
           (define prefix (format "~a." (symbol->string (syntax-e int-id))))
           (for*/fold ([ht base-ht]) ([space-sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                                      #:when (use-space? space-sym spaces-mode spaces)
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
                   (define ext-ext-id (datum->syntax ext-id sym))
                   (hash-set ht sym (merge-ext+int+rule
                                     ext-ext-id
                                     (hash-ref ht sym #f)
                                     (make-ext+int+rule ext-ext-id id '#:only (hasheq space-sym
                                                                                      (datum->syntax #f space-sym)))))]
                  [else ht])]
               [else ht]))]))
      (syntax-parse ex
        #:datum-literals (combine-out all-spaces-out all-from-out for-meta for-label
                                      only-spaces-out except-spaces-out all-spaces-defined-out
                                      except-out)
        [(combine-out ex ...)
         (for/fold ([ht ht]) ([ex (in-list (syntax->list #'(ex ...)))])
           (loop ex ht except-ht spaces spaces-mode))]
        [(all-spaces-out o ...)
         (for/fold ([ht ht]) ([o (in-list (syntax->list #'(o ...)))])
           (define-values (ext-id int-id)
             (syntax-parse o
               [(int-id ext-id) (values #'ext-id #'int-id)]
               [_:identifier (values o o)]))
           (add-name-at-all-spaces ht ext-id int-id spaces spaces-mode))]
        [(only-spaces-out ex space ...)
         (define new-spaces
           (for/hasheq ([sp (in-list (syntax->list #'(space ...)))])
             (values (syntax-e sp) sp)))
         (cond
           [(not spaces-mode)
            (loop #'ex ht except-ht new-spaces '#:only)]
           [else
            (for ([sp (in-hash-keys spaces)])
              (unless (hash-ref new-spaces sp #f)
                (raise-syntax-error #f
                                    "space not included in nested modification"
                                    (hash-ref spaces sp))))
            (cond
              [(eq? spaces-mode '#:only)
               (loop #'ex ht except-ht spaces '#:only)]
              [else
               (define keep-spaces (for/fold ([new-spaces new-spaces]) ([sp (in-hash-keys spaces)])
                                     (hash-remove new-spaces sp)))
               (loop #'ex ht except-ht keep-spaces '#:only)])])]
        [(except-spaces-out ex space ...)
         (define new-spaces
           (for/hasheq ([sp (in-list (syntax->list #'(space ...)))])
             (values (syntax-e sp) sp)))
         (cond
           [(not spaces-mode)
            (loop #'ex ht except-ht new-spaces '#:except)]
           [else
            (for ([sp (in-hash-keys spaces)])
              (when (hash-ref new-spaces sp #f)
                (raise-syntax-error #f
                                    "space excluded in nested modification"
                                    (hash-ref spaces sp))))
            (cond
              [(eq? spaces-mode '#:only)
               (loop #'ex ht except-ht spaces '#:only)]
              [else
               (define remove-spaces (for/fold ([new-spaces new-spaces]) ([(sp id) (in-hash spaces)])
                                       (hash-set new-spaces sp id)))
               (loop #'ex ht except-ht remove-spaces '#:except)])])]
        [(all-spaces-defined-out)
         (for/fold ([ht ht]) ([sym (in-list (syntax-bound-symbols ex))])
           (define id (datum->syntax ex sym))
           (define use-spaces
             (for/hasheq ([space-sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                          #:when (use-space? space-sym spaces-mode spaces)
                          #:do [(define space-id
                                  (if space-sym
                                      ((make-interned-syntax-introducer space-sym) id)
                                      id))]
                          #:when (if space-sym
                                     (identifier-distinct-binding* space-id id)
                                     (identifier-binding* space-id))
                          #:when (identifier-distinct-binding* space-id
                                                               (expose space-id)))
               (values space-sym #t)))
           (cond
             [(null? use-spaces) ht]
             [else (add-name-at-all-spaces ht id id use-spaces '#:only)]))]
        [(except-out starting-e exclude-e)
         (define all-except-ht (loop #'exclude-e except-ht #hasheq() spaces spaces-mode))
         (loop #'starting-e ht all-except-ht spaces spaces-mode)]
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

;; Checks consistency while merging. Merging is complicated, because
;; we want to keep `#:except` forms as they are, instead of trying to
;; pin down all relevant spaces at this point; an `#:only` can subsume
;; an `#:except`, though, while `#:except`s and `#:only`s can be
;; added, `#:module`s must be merged, and a combination of `#:only`s
;; and `#:except` might need to be converted into a `#:module` and
;; `#:except` or `#:only`. Finally, it's possible for two `#:except`s
;; to not be mergeable, because they are on identifiers that don't
;; have exactly the same scopes (so different bindings might be
;; discovered).
(define-for-syntax (merge-ext+int+rule ext-id old new)
  (cond
    [(not old) new]
    [else
     (define old-int (cadr old))
     (define new-int (cadr new))
     ;; `lookup` used for checking consistency:
     (define (lookup ext+int+rule space-sym)
       (define int-id (cadr ext+int+rule))
       (let loop ([rule (cddr ext+int+rule)])
         (cond
           [(null? rule) int-id]
           [(eq? (car rule) '#:only)
            (and (memq space-sym (cdr rule))
                 int-id)]
           [(eq? (car rule) '#:except)
            (and (not (memq space-sym (cdr rule)))
                 int-id)]
           [(eq? (car rule) '#:space)
            (or (for/or ([space+id (in-list (cadr rule))])
                  (and (eq? (car space+id) space-sym)
                       (cadr space+id)))
                (loop (cddr rule)))])))
     ;; check consistency over all spaces:
     (for ([space-sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))])
       (define old-id (lookup old space-sym))
       (define new-id (lookup new space-sym))
       (when (and old-id new-id)
         (define intro (if space-sym
                           (make-interned-syntax-introducer space-sym)
                           (lambda (x mode) x)))
         (when (not (free-identifier=? (intro old-int 'add) (intro new-int 'add)))
           (raise-syntax-error #f
                               (string-append
                                "duplicate export name with different bindings"
                                (if space-sym
                                    (format "\n  in space: ~a" space-sym)
                                    ""))
                               ext-id))))
     ;; merge
     (define (need-int? ext+int+rule)
       (let loop ([l (cddr ext+int+rule)])
         (cond
           [(null? l) #t]
           [(eq? (car l) '#:only) (null? (cdr l))]
           [(eq? (car l) '#:space) (loop (cddr l))]
           [else #f])))
     (define use-int (if (need-int? old) old-int new-int))
     (define (to-set l) (for/hasheq ([s (in-list l)]) (values s #t)))
     (define (add-spaces-to-table int-id spaces space-table)
       (for/fold ([ht space-table]) ([sp (in-list spaces)])
         (hash-set ht sp int-id)))
     (define (add-to-table space-table space+ids)
       (for/fold ([ht space-table]) ([space+id (in-list space+ids)])
         (hash-set ht (car space+id) (cadr space+id))))
     (define (add-table space-table rule)
       (if (= 0 (hash-count space-table))
           rule
           (list* '#:space (hash-map space-table list) rule)))
     (list* (car old)
            use-int
            (let loop ([old (cddr old)] [new (cddr new)] [space-table #hasheq()])
              (cond
                [(null? old) (add-table space-table new)]
                [(null? new) (add-table space-table old)]
                [(eq? (car new) '#:space)
                 (loop old (cddr new) (add-to-table space-table (cadr new)))]
                [(eq? (car old) '#:except)
                 (cond
                   [(eq? (car new) '#:except)
                    (unless (bound-identifier=? old-int new-int)
                      (raise-syntax-error #f
                                          "additional space exceptions uses a different identifier"
                                          new-int))
                    (add-table space-table
                               (cons '#:except (let ([ht (to-set (cdr old))])
                                                 (for/list ([sp (in-list (cdr new))]
                                                            #:when (hash-ref ht sp #f))
                                                   sp))))]
                   [(eq? (car new) '#:only)
                    (cond
                      [(bound-identifier=? old-int new-int)
                       (define spaces (hash-keys (for/fold ([ht (to-set (cdr old))])
                                                           ([new-sym (in-list (cdr new))])
                                                   (hash-remove ht new-sym))))
                       (add-table space-table
                                  (if (null? spaces)
                                      null
                                      (cons '#:except spaces)))]
                      [else
                       (loop old (cddr new) (add-spaces-to-table new-int (cdr new) space-table))])]
                   [else
                    (loop old (cddr new) (add-to-table space-table (cadr new)))])]
                [(eq? (car old) '#:only)
                 (cond
                   [(eq? (car new) '#:only)
                    (cond
                      [(bound-identifier=? old-int new-int)
                       (add-table space-table
                                  (cons '#:only (hash-keys (to-set (append (cdr old) (cdr new))))))]
                      [else
                       (loop new (cddr old) (add-spaces-to-table old-int (cdr old) space-table))])]
                   [else (loop new old space-table)])]
                [else (loop new old space-table)])))]))
