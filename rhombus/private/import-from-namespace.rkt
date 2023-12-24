#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol
                     "import-cover.rkt"
                     "id-binding.rkt")
         "space-in.rkt")

;; Implements a subset of `racket` require to adjust a mapping of keys
;; to values for a namespace used as an import

(provide (for-syntax convert-require-from-namespace
                     check-allowed-for-dotted
                     space-excluded?
                     expose-spaces
                     expose-spaces-with-rule
                     find-identifer-in-spaces
                     close-over-extensions))

(begin-for-syntax
  ;; inputs:
  ;;   - r: the parsed `require` form
  ;;   - ht: sym -> (list (cons id space) ...) = content of namespace in hash-table form
  ;;   - covered-ht: threaded through conversions to make sure every identifier is used
  ;;   - accum?: whether to add to `covered-ht` or complain about missing
  ;;   - phase-shink-of?
  ;;   - only-space-sym: #f means all spaces
  ;; results are
  ;;   - ht: sym -> (list (cons id space) ...) = adjusted (by renames, excepts, etc.) content
  ;;   - exposed-ht: sym -> id, where `id` is the identifier to bind (without a prefix)
  ;;   - new-covered-ht: updated coverage (when `accum?` is true)
  ;;   - as-is?: true means `ht` is as given, and `exposed-ht` is empty
  (define (convert-require-from-namespace r ht covered-ht accum? phase-shift-ok? only-space-sym for-singleton?)
    (define included-str (if for-singleton? "nested within the imported name" "included"))
    (let extract ([r r] [ht ht] [step 0])
      (define (root) (values ht #hasheq() covered-ht #t))
      (syntax-parse r
        #:datum-literals (rename-in only-in except-in expose-in for-meta for-label
                                    only-spaces-in except-spaces-in rhombus-prefix-in
                                    only-space-in only-meta-in)
        [#f (root)]
        [(rename-in mp [orig bind] ...)
         (define-values (new-ht new-expose-ht covered-ht as-is?) (extract #'mp ht (add1 step)))
         (define-values (renames rename-bind-ids)
           (for/fold ([renames #hasheq()]
                      [rename-bind-ids #hasheq()])
                     ([orig-s (in-list (syntax->list #'(orig ...)))]
                      [bind-s (in-list (syntax->list #'(bind ...)))])
               (define orig (syntax-e orig-s))
               (define bind (syntax-e bind-s))
               (cond
                 [(hash-ref new-ht orig #f)
                  (when (hash-ref renames orig #f)
                    (raise-syntax-error 'import "duplicate rename for identifier" orig-s))
                  (values (hash-set renames orig bind)
                          (hash-set rename-bind-ids bind bind-s))]
                 [(or accum? (covered? covered-ht orig step))
                  (values renames
                          rename-bind-ids)]
                 [else
                  (raise-syntax-error 'import (string-append "identifier to rename is not " included-str) orig-s)])))
         (define-values (pruned-ht pruned-expose-ht)
           (for/fold ([ht new-ht] [expose-ht new-expose-ht])
                     ([(orig bind) (in-hash renames)])
             (values (hash-remove ht orig)
                     (hash-remove expose-ht orig))))
         (for/fold ([ht pruned-ht] [expose-ht pruned-expose-ht] [covered-ht covered-ht] [as-is? #f])
                   ([(orig bind) (in-hash renames)])
           (values (hash-set ht bind (hash-ref new-ht orig))
                   (if (hash-ref new-expose-ht orig #f)
                       (hash-set expose-ht bind (hash-ref rename-bind-ids bind))
                       expose-ht)
                   (cover covered-ht orig step)
                   #f))]
        [(only-in mp id ...)
         (define-values (new-ht new-expose-ht covered-ht as-is?) (extract #'mp ht (add1 step)))
         (for/fold ([ht #hasheq()]
                    [expose-ht #hasheq()]
                    [covered-ht covered-ht]
                    #:result (values ht expose-ht covered-ht #f))
                   ([id-s (in-list (syntax->list #'(id ...)))])
           (define id (syntax-e id-s))
           (cond
             [(hash-ref new-ht id #f)
              => (lambda (v)
                   (values (hash-set ht id v)
                           (let ([as-id (hash-ref new-expose-ht id #f)])
                             (if as-id
                                 (hash-set expose-ht id as-id)
                                 expose-ht))
                           (cover covered-ht id step)))]
             [(or accum? (covered? covered-ht id step))
              (values ht expose-ht covered-ht)]
             [else
              (raise-syntax-error 'import (string-append "identifier is not " included-str) id-s)]))]
        [(except-in mp id ...)
         (define-values (new-ht new-expose-ht covered-ht as-is?) (extract #'mp ht (add1 step)))
         (for/fold ([ht new-ht]
                    [expose-ht new-expose-ht]
                    [covered-ht covered-ht]
                    #:result (values ht expose-ht covered-ht #f))
                   ([id-s (in-list (syntax->list #'(id ...)))])
           (define id (syntax-e id-s))
           (cond
             [(hash-ref new-ht id #f) (values (hash-remove ht id)
                                              (hash-remove expose-ht id)
                                              (cover covered-ht id step))]
             [(or accum? (covered? covered-ht id step))
              (values ht expose-ht covered-ht)]
             [else
              (raise-syntax-error 'import (string-append "identifier to exclude is not " included-str) id-s)]))]
        [(expose-in mp id ...)
         (define-values (new-ht new-expose-ht covered-ht as-is?) (extract #'mp ht (add1 step)))
         (define-values (exposed-expose-ht exposed-covered-ht)
           (for/fold ([expose-ht new-expose-ht]
                      [covered-ht covered-ht])
                     ([id-s (in-list (syntax->list #'(id ...)))])
             (define id (syntax-e id-s))
             (cond
               [(hash-ref new-ht id #f)
                (values (hash-set expose-ht id id-s)
                        (cover covered-ht id step))]
               [(or accum? (covered? covered-ht id step))
                (values expose-ht
                        covered-ht)]
               [else
                (raise-syntax-error 'import (string-append "identifier to expose is not " included-str) id-s)])))
         (values new-ht exposed-expose-ht exposed-covered-ht #f)]
        [(for-meta phase mp)
         (if (or phase-shift-ok?
                 (eq? (syntax-e #'phase) 0))
             (extract #'mp ht step)
             (raise-syntax-error 'import "cannot shift phase of namespace content" r))]
        [(for-label mp)
         (if phase-shift-ok?
             (extract #'mp ht step)
             (raise-syntax-error 'import "cannot shift phase of namespace content" r))]
        [(only-meta-in phase mp)
         (if (or phase-shift-ok?
                 (eqv? 0 (syntax-e #'phase)))
             (extract #'mp ht step)
             (raise-syntax-error 'import "cannot prune phase of namespace content" r))]
        [(rhombus-prefix-in mp . _) (extract #'mp ht step)]
        [((~and mode (~or* only-spaces-in except-spaces-in)) mp a-space ...)
         (define-values (new-ht new-expose-ht covered-ht as-is?) (extract #'mp ht (add1 step)))
         (define the-spaces
           (for/hasheq ([a-space (in-list (syntax->list #'(a-space ...)))])
             (values (syntax-e a-space) #t)))
         (define keep? (free-identifier=? #'mode #'only-spaces-in))
         (define pruned-ht
           (for/hasheq ([(key id/id+spaces) (in-hash new-ht)]
                        #:do [(define new-id+spaces
                                (for/list ([id+space (in-list (expose-spaces id/id+spaces only-space-sym))]
                                           #:when (if keep?
                                                      (hash-ref the-spaces (cdr id+space) #f)
                                                      (not (hash-ref the-spaces (cdr id+space) #f))))
                                  id+space))]
                        #:when (pair? new-id+spaces))
             (values key new-id+spaces)))
         (define pruned-expose-ht
           (for/hasheq ([(k k-id) (in-hash new-expose-ht)]
                        #:when (hash-ref pruned-ht k #f))
             (values k k-id)))
         (values pruned-ht pruned-expose-ht covered-ht #f)]
        [(only-space-in space mp) ;; redundant for namespaces
         (extract #'mp ht step)]
        [_ (raise-syntax-error 'import
                               "don't know how to convert"
                               r)]))))

(define-for-syntax (check-allowed-for-dotted r)
  (let loop ([r r])
    (syntax-parse r
      #:datum-literals (rename-in only-in except-in expose-in for-meta for-label
                                  only-spaces-in except-spaces-in rhombus-prefix-in
                                  only-space-in only-meta-in)
      [#f (void)]
      [((~or* rename-in only-in except-in expose-in only-spaces-in except-spaces-in rhombus-prefix-in) mp . _)
       (loop #'mp)]
      [((~or* for-meta for-label) . _)
       (raise-syntax-error 'import "cannot shift phase with dotted-import shorthand" r)]
      [(only-space-in space mp)
       (loop #'mp)]
      [(only-meta-in phase mp)
       (loop #'mp)]
      [_ (raise-syntax-error 'import "don't know how to check" r)])))

;; used for singleton imports
(define-for-syntax (space-excluded? space-sym r)
  (let loop ([r r])
    (syntax-parse r
      #:datum-literals (rename-in only-in except-in expose-in for-meta for-label
                                  only-spaces-in except-spaces-in rhombus-prefix-in
                                  only-space-in only-meta-in)
      [#f #f]
      [((~or* rename-in only-in except-in expose-in rhombus-prefix-in) mp . _)
       (loop #'mp)]
      [((~and mode (~or* only-spaces-in except-spaces-in)) mp a-space ...)
       (define the-spaces
         (for/hasheq ([a-space (in-list (syntax->list #'(a-space ...)))])
           (values (syntax-e a-space) #t)))
       (define keep? (free-identifier=? #'mode #'only-spaces-in))
       (or (if keep?
               (not (hash-ref the-spaces space-sym #f))
               (hash-ref the-spaces space-sym #f))
           (loop #'mp))]
      [(only-meta-in phase mp) (loop #'mp)]
      [_ (raise-syntax-error 'import "don't know how to check exclusion" r)])))

(define-for-syntax (expose-spaces id/id+spaces only-space-sym)
  (if (identifier? id/id+spaces)
      (find-identifer-in-spaces id/id+spaces only-space-sym)
      id/id+spaces))

(define-for-syntax (find-identifer-in-spaces id-in only-space
                                             #:keep? [keep? (lambda (space-sym) #t)])
  ;; find all spaces where the identifier is bound
  (for*/fold ([id+spaces '()]) ([space-sym (in-list (if only-space
                                                        (list only-space)
                                                        (cons #f (syntax-local-module-interned-scope-symbols))))]
                                #:when (keep? space-sym))
    (define intro (if space-sym
                      (make-interned-syntax-introducer space-sym)
                      (lambda (x mode) x)))
    (define id (intro id-in 'add))
    (if (and (identifier-binding* id)
             (not (ormap (lambda (id+space)
                           (free-identifier=? (car id+space) id))
                         id+spaces)))
        (cons (cons id space-sym) id+spaces)
        id+spaces)))

(define-for-syntax (expose-spaces-with-rule id rule)
  (let loop ([rule rule])
    (syntax-parse rule
      #:datum-literals (only)
      [() id]
      [(#:space ([space space-id] ...) . rule-rest)
       (append
        (map cons
             (syntax->list #'(space-id ...))
             (map syntax-e (syntax->list #'(space ...))))
        (loop #'rule-rest))]
      [(#:only) ; shortcut for zero spaces
       null]
      [((~and mode (~or* #:only #:except)) space ...)
       (define spaces (for/hasheq ([space (in-list (syntax->list #'(space ...)))])
                        (values (syntax-e space) #t)))
       (find-identifer-in-spaces
        id #f
        #:keep? (if (eq? (syntax-e #'mode) '#:only)
                    (lambda (space-sym) (hash-ref spaces space-sym #f))
                    (lambda (space-sym) (not (hash-ref spaces space-sym #f)))))])))

(define-for-syntax (close-over-extensions expose-ht ht)
  (let loop ([expose-ht expose-ht]
             [expose-ks (sort (hash-keys expose-ht) symbol<?)]
             [ks (sort (hash-keys ht) symbol<?)])
    (cond
      [(null? ks) expose-ht]
      [(null? expose-ks) expose-ht]
      [else
       (define ek (car expose-ks))
       (define str (format "~a." (symbol->immutable-string ek)))
       (let e-loop ([expose-ht expose-ht]
                    [ks ks])
         (cond
           [(null? ks) (loop expose-ht (cdr expose-ks) ks)]
           [(or (eq? (car ks) ek)
                (symbol<? (car ks) ek)
                (hash-ref expose-ht (car ks) #f))
            (e-loop expose-ht (cdr ks))]
           [else
            (define k-str (symbol->immutable-string (car ks)))
            (define len (string-length str))
            (cond
              [(and ((string-length k-str) . > . len)
                    (equal? (substring k-str 0 len) str))
               (e-loop (hash-set expose-ht (car ks)
                                 ;; synthesize name using context of the prefix name
                                 (datum->syntax (hash-ref expose-ht ek)
                                                (string->symbol
                                                 (string-append str (substring k-str len)))))
                       (cdr ks))]
              [else (loop expose-ht (cdr expose-ks) ks)])]))])))
