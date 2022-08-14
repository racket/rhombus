#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/phase+space))

;; Implements a subset of `racket` require to adjust a mapping of keys
;; to values for a name root used as an import

(provide (for-syntax convert-require-from-root))

(begin-for-syntax
  (define (convert-require-from-root r ht)
    (let extract ([r r] [ht ht])
      (define (root) (values ht #hasheq()))
      (syntax-parse r
        [((~literal import-root) . _) (root)]
        [((~literal rename-in) mp [orig bind] ...)
         (define-values (new-ht new-expose-ht) (extract #'mp ht))
         (define renames
           (for/fold ([renames #hasheq()])
                     ([orig-s (in-list (syntax->list #'(orig ...)))]
                      [bind-s (in-list (syntax->list #'(bind ...)))])
               (define orig (syntax-e orig-s))
               (define bind (syntax-e bind-s))
               (cond
                 [(hash-ref new-ht orig #f)
                  => (lambda (old)
                       (unless (hash-ref renames orig #f)
                         (raise-syntax-error 'import "duplicate rename for identifier" orig-s))
                       (hash-set renames old bind))]
                 [else
                  (raise-syntax-error 'import "identifier to rename is not included" orig-s)])))
         (define-values (pruned-ht pruned-expose-ht)
           (for/fold ([ht new-ht] [expose-ht new-expose-ht])
                     ([(orig bind) (in-hash renames)])
             (values (hash-remove ht orig)
                     (hash-remove expose-ht orig))))
         (for/fold ([ht pruned-ht] [expose-ht pruned-expose-ht])
                   ([(orig bind) (in-hash renames)])
           (values (hash-set ht bind (hash-ref new-ht orig))
                   (if (hash-ref new-expose-ht orig #f)
                       (hash-set expose-ht bind #t)
                       expose-ht)))]
        [((~literal only-in) mp id ...)
         (define-values (new-ht new-expose-ht) (extract #'mp ht))
         (for/fold ([ht #hasheq()]
                    [expose-ht #hasheq()])
                   ([id-s (in-list (syntax->list #'(id ...)))])
           (define id (syntax-e id-s))
           (cond
             [(hash-ref new-ht id #f)
              => (lambda (v)
                   (values (hash-set ht id v)
                           (if (hash-ref new-expose-ht id #f)
                               (hash-set expose-ht id #t)
                               expose-ht)))]
             [else
              (raise-syntax-error 'import "identifier is not included" id-s)]))]
        [((~literal except-in) mp id ...)
         (define-values (new-ht new-expose-ht) (extract #'mp ht))
         (for/fold ([ht new-ht]
                    [expose-ht new-expose-ht])
                   ([id-s (in-list (syntax->list #'(id ...)))])
           (define id (syntax-e id-s))
           (cond
             [(hash-ref new-ht id #f) (values (hash-remove ht id)
                                              (hash-remove expose-ht id))]
             [else
              (raise-syntax-error 'import "identifier to exclude is not included" id-s)]))]
        [((~literal expose-in) mp id ...)
         (define-values (new-ht new-expose-ht) (extract #'mp ht))
         (values new-ht
                 (for/fold ([expose-ht new-expose-ht])
                           ([id-s (in-list (syntax->list #'(id ...)))])
                   (define id (syntax-e id-s))
                   (cond
                     [(hash-ref new-ht id #f) (hash-set expose-ht id #t)]
                     [else
                      (raise-syntax-error 'import "identifier to expose is not included" id-s)])))]
        [((~literal for-meta) phase mp)
         (if (eq? (syntax-e #'phase) 0)
             (extract #'mp ht)
             (raise-syntax-error 'import "cannot shift phase of name-root content" r))]
        [((~literal for-label) mp)
         (raise-syntax-error 'import "cannot shift phase of name-root content" r)]
        [((~literal rhombus-prefix-in) mp name) (extract #'mp ht)]
        [_ (raise-syntax-error 'import
                               "don't know how to convert"
                               r)]))))
