#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/phase+space
                     "import-cover.rkt")
         "space-in.rkt")

;; Implements a subset of `racket` require to adjust a mapping of keys
;; to values for a namespace used as an import

(provide (for-syntax convert-require-from-namespace
                     check-allowed-for-dotted
                     space-excluded?
                     expose-spaces
                     find-identifer-in-spaces))

(begin-for-syntax
  (define (convert-require-from-namespace r ht covered-ht accum? phase-shift-ok? only-space-sym)
    (let extract ([r r] [ht ht] [step 0])
      (define (root) (values ht #hasheq() covered-ht #t))
      (syntax-parse r
        #:datum-literals (rename-in only-in except-in expose-in for-meta for-label
                                    only-spaces-in except-spaces-in rhombus-prefix-in only-space-in)
        [#f (root)]
        [(rename-in mp [orig bind] ...)
         (define-values (new-ht new-expose-ht covered-ht as-is?) (extract #'mp ht (add1 step)))
         (define renames
           (for/fold ([renames #hasheq()])
                     ([orig-s (in-list (syntax->list #'(orig ...)))]
                      [bind-s (in-list (syntax->list #'(bind ...)))])
               (define orig (syntax-e orig-s))
               (define bind (syntax-e bind-s))
               (cond
                 [(hash-ref new-ht orig #f)
                  (when (hash-ref renames orig #f)
                    (raise-syntax-error 'import "duplicate rename for identifier" orig-s))
                  (hash-set renames orig bind)]
                 [(or accum? (covered? covered-ht orig step))
                  renames]
                 [else
                  (raise-syntax-error 'import "identifier to rename is not included" orig-s)])))
         (define-values (pruned-ht pruned-expose-ht)
           (for/fold ([ht new-ht] [expose-ht new-expose-ht])
                     ([(orig bind) (in-hash renames)])
             (values (hash-remove ht orig)
                     (hash-remove expose-ht orig))))
         (for/fold ([ht pruned-ht] [expose-ht pruned-expose-ht] [covered-ht covered-ht] [as-is? #f])
                   ([(orig bind) (in-hash renames)])
           (values (hash-set ht bind (hash-ref new-ht orig))
                   (if (hash-ref new-expose-ht orig #f)
                       (hash-set expose-ht bind #t)
                       expose-ht)
                   (cover covered-ht orig step)
                   #f))]
        [(only-in mp id ...)
         (define-values (new-ht new-expose-ht covered-ht as-is?) (extract #'mp ht (add1 step)))
         (for/fold ([ht #hasheq()]
                    [expose-ht #hasheq()]
                    [covered-ht covered-ht])
                   ([id-s (in-list (syntax->list #'(id ...)))])
           (define id (syntax-e id-s))
           (cond
             [(hash-ref new-ht id #f)
              => (lambda (v)
                   (values (hash-set ht id v)
                           (if (hash-ref new-expose-ht id #f)
                               (hash-set expose-ht id #t)
                               expose-ht)
                           (cover covered-ht id step)
                           #f))]
             [(or accum? (covered? covered-ht id step))
              (values ht expose-ht covered-ht #f)]
             [else
              (raise-syntax-error 'import "identifier is not included" id-s)]))]
        [(except-in mp id ...)
         (define-values (new-ht new-expose-ht covered-ht as-is?) (extract #'mp ht (add1 step)))
         (for/fold ([ht new-ht]
                    [expose-ht new-expose-ht]
                    [covered-ht covered-ht]
                    [as-is? #f])
                   ([id-s (in-list (syntax->list #'(id ...)))])
           (define id (syntax-e id-s))
           (cond
             [(hash-ref new-ht id #f) (values (hash-remove ht id)
                                              (hash-remove expose-ht id)
                                              (cover covered-ht id step)
                                              #f)]
             [(or accum? (covered? covered-ht id step))
              (values ht expose-ht covered-ht #f)]
             [else
              (raise-syntax-error 'import "identifier to exclude is not included" id-s)]))]
        [(expose-in mp id ...)
         (define-values (new-ht new-expose-ht covered-ht as-is?) (extract #'mp ht (add1 step)))
         (define-values (exposed-expose-ht exposed-covered-ht)
           (for/fold ([expose-ht new-expose-ht]
                      [covered-ht covered-ht])
                     ([id-s (in-list (syntax->list #'(id ...)))])
             (define id (syntax-e id-s))
             (cond
               [(hash-ref new-ht id #f)
                (values (hash-set expose-ht id #t)
                        (cover covered-ht id step))]
               [(or accum? (covered? covered-ht id step))
                (values expose-ht
                        covered-ht)]
               [else
                (raise-syntax-error 'import "identifier to expose is not included" id-s)])))
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
        [(rhombus-prefix-in mp name) (extract #'mp ht step)]
        [((~and mode (~or only-spaces-in except-spaces-in)) mp a-space ...)
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
           (for/hasheq ([k (in-hash-keys new-expose-ht)]
                        #:when (hash-ref pruned-ht k #f))
             (values k #t)))
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
                                  only-spaces-in except-spaces-in rhombus-prefix-in only-space-in)
      [#f (void)]
      [((~or rename-in only-in except-in expose-in only-spaces-in except-spaces-in rhombus-prefix-in) mp . _)
       (loop #'mp)]
      [((~or for-meta for-label) . _)
       (raise-syntax-error 'import "cannot shift phase with dotted-import shorthand" r)]
      [(only-space-in space mp)
       (loop #'mp)]
      [_ (raise-syntax-error 'import "don't know how to check" r)])))

;; used for singleton imports
(define-for-syntax (space-excluded? space-sym r)
  (let loop ([r r])
    (syntax-parse r
      #:datum-literals (rename-in only-in except-in expose-in for-meta for-label
                                  only-spaces-in except-spaces-in rhombus-prefix-in only-space-in)
      [#f #f]
      [((~or rename-in only-in except-in expose-in) mp . _)
       (loop #'mp)]
      [((~and mode (~or only-spaces-in except-spaces-in rhombus-prefix-in)) mp a-space ...)
       (define the-spaces
         (for/hasheq ([a-space (in-list (syntax->list #'(a-space ...)))])
           (values (syntax-e a-space) #t)))
       (define keep? (free-identifier=? #'mode #'only-spaces-in))
       (or (if keep?
               (not (hash-ref the-spaces space-sym #f))
               (hash-ref the-spaces space-sym #f))
           (loop #'mp))]
      [_ (raise-syntax-error 'import "don't know how to check exclusion" r)])))

(define-for-syntax (expose-spaces id/id+spaces only-space-sym)
  (if (identifier? id/id+spaces)
      (find-identifer-in-spaces id/id+spaces only-space-sym)
      id/id+spaces))

(define-for-syntax (find-identifer-in-spaces id-in only-space)
  ;; find all spaces where the identifier is bound
  (for*/fold ([id+spaces '()]) ([space-sym (in-list (if only-space
                                                        (list only-space)
                                                        (cons #f (syntax-local-module-interned-scope-symbols))))])
    (define intro (if space-sym
                      (make-interned-syntax-introducer space-sym)
                      (lambda (x mode) x)))
    (define id (intro id-in 'add))
    (if (and (identifier-binding id)
             (not (ormap (lambda (id+space)
                           (free-identifier=? (car id+space) id))
                         id+spaces)))
        (cons (cons id space-sym) id+spaces)
        id+spaces)))
