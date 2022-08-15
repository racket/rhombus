#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/phase+space))

;; Convert a subset of `racket` require clauses to `#%require` clauses,
;; including the use of `portal` for a prefixed import

(provide (for-syntax lower-require-clause)
         expose-in
         import-dotted)

(define-syntax expose-in #f)
(define-syntax import-dotted #f)

(begin-for-syntax
  (define (lower-require-clause r mod-path prefix-id)
    (define (expose v) (vector v))
    (define (expose? v) (vector? v))
    (define (expose-id v) (vector-ref v 0))
    (define-values (r-phase+spaces core-r lower-r phase-shift renames revnames only-mentioned?)
      (let extract ([r r])
        (define (root)
          (define phase+spaces (map car (syntax-local-module-exports (syntax->datum mod-path))))
          (values phase+spaces mod-path mod-path 0 #hasheq() #hasheq() #f))
        (syntax-parse r
          [#f (root)]
          [((~literal rename-in) mp [orig bind] ...)
           (define-values (phase+spaces core-p p shift renames revnames only-mentioned?) (extract #'mp))
           (define-values (new-renames new-revnames)
             (for/fold ([renames renames]
                        [revnames revnames])
                       ([orig-s (in-list (syntax->list #'(orig ...)))]
                        [bind-s (in-list (syntax->list #'(bind ...)))])
               (define orig (syntax-e orig-s))
               (define bind (syntax-e bind-s))
               (define true-orig (hash-ref revnames orig #f))
               (cond
                 [true-orig
                  (cond
                    [(hash-ref renames true-orig #f)
                     => (lambda (old-orig)
                          (values (hash-set renames true-orig (if (box? old-orig)
                                                                  (box bind-s)
                                                                  bind-s))
                                  (hash-set (hash-remove revnames orig) bind true-orig)))]
                    [else
                     (raise-syntax-error 'import "identifier to rename is excluded" orig-s)])]
                 [else
                  (when only-mentioned?
                    (raise-syntax-error 'import "identifier to rename is not included" orig-s))
                  (values (hash-set renames orig bind-s)
                          (hash-set revnames bind orig))])))
           (values phase+spaces core-p p shift new-renames new-revnames only-mentioned?)]
          [((~literal only-in) mp id ...)
           (define-values (phase+spaces core-p p shift renames revnames only-mentioned?) (extract #'mp))
           (define-values (new-renames new-revnames)
             (for/fold ([new-renames #hasheq()]
                        [new-revnames #hasheq()])
                       ([id-s (in-list (syntax->list #'(id ...)))])
               (define id (syntax-e id-s))
               (define orig (hash-ref revnames id #f))
               (cond
                 [orig
                  (cond
                    [(hash-ref renames orig #f)
                     => (lambda (target)
                          (values (hash-set new-renames orig target)
                                  (hash-set new-revnames id orig)))]
                    [else
                     (raise-syntax-error 'import "identifier to include was previously excluded" id-s)])]
                 [else
                  (when only-mentioned?
                    (raise-syntax-error 'import "identifier to include was not previously included" id-s))
                  (values (hash-set new-renames id id-s)
                          (hash-set new-revnames id id))])))
           (values phase+spaces core-p p shift new-renames new-revnames #t)]
          [((~literal except-in) mp id ...)
           (define-values (phase+spaces core-p p shift renames revnames only-mentioned?) (extract #'mp))
           (define-values (new-renames new-revnames)
             (for/fold ([renames renames]
                        [revnames revnames])
                       ([id-s (in-list (syntax->list #'(id ...)))])
               (define id (syntax-e id-s))
               (define orig (hash-ref revnames id #f))
               (cond
                 [orig
                  (cond
                    [(hash-ref renames orig #f)
                     (if only-mentioned?
                         (values (hash-remove renames orig)
                                 (hash-remove revnames id))
                         (values (hash-set renames orig #f)
                                 revnames))]
                    [else
                     (raise-syntax-error 'import "identifier to exclude was previously excluded" id-s)])]
                 [else
                  (when only-mentioned?
                    (raise-syntax-error 'import "identifier to exclude was not previously included" id-s))
                  (values (hash-set renames id #f)
                          (hash-set revnames id id))])))
           (values phase+spaces core-p p shift new-renames new-revnames only-mentioned?)]
          [((~literal expose-in) mp id ...)
           (define-values (phase+spaces core-p p shift renames revnames only-mentioned?) (extract #'mp))
           (define-values (new-renames new-revnames)
             (for/fold ([renames renames]
                        [revnames revnames])
                       ([id-s (in-list (syntax->list #'(id ...)))])
               (define id (syntax-e id-s))
               (define orig (hash-ref revnames id #f))
               (cond
                 [(not orig)
                  (when only-mentioned?
                    (raise-syntax-error 'import "identifier to expose was not previously included" id-s))
                  (values (hash-set renames id (expose id-s))
                          (hash-set revnames id id))]
                 [(hash-ref renames orig #f)
                  (values (hash-set renames orig (expose id-s))
                          revnames)]
                 [else
                  (raise-syntax-error 'import "identifier to expose was previously excluded" id-s)])))
           (values phase+spaces core-p p shift new-renames new-revnames only-mentioned?)]
          [((~literal for-meta) phase mp)
           (define-values (phase+spaces core-p p shift renames revnames only-mentioned?) (extract #'mp))
           (define new-shift (and shift (syntax-e #'phase)
                                  (+ shift (syntax-e #'phase))))
           (values phase+spaces core-p p new-shift renames revnames only-mentioned?)]
          [((~literal for-label) mp)
           (define-values (phase+spaces core-p p shift renames revnames only-mentioned?) (extract #'mp))
           (define new-shift #f)
           (values phase+spaces core-p p new-shift renames revnames only-mentioned?)]
          [((~literal rhombus-prefix-in) mp name) (extract #'mp)]
          [_ (raise-syntax-error 'import
                                 "don't know how to lower"
                                 r)])))
    (define (strip-prefix r mp)
      (let strip ([r r])
        (syntax-parse r
          #:literals (rename-in only-in except-in expose-in for-label)
          [#f mp]
          [((~and tag (~or rename-in only-in except-in expose-in for-label)) mp . rest)
           #`(tag #,(strip #'mp) . rest)]
          [((~and tag (~literal for-meta)) phase mp)
           #`(tag phase #,(strip #'mp))]
          [((~literal rhombus-prefix-in) mp name) (strip #'mp)]
          [_ (raise-syntax-error 'import "don't know how to strip" r)])))
    (define (plain-id v) (if (expose? v) (expose-id v) v))
    (define (make-ins for-expose?)
      (cond
        [only-mentioned?
         (cond
           [(for/and ([(k v) (in-hash renames)])
              (eq? k v))
            ;; only specific ids, none exposed or renamed
            (if for-expose?
                '()
                (list #'(only #,lower-r
                              #,@(for/list ([k (in-hash-keys renames)])
                                   k))))]
           [else
            ;; specific ids, some exposed or renamed
            (for/list ([(k v) (in-hash renames)]
                       #:when (or (not for-expose?)
                                  (expose? v)))
              #`(rename #,lower-r #,(plain-id v) #,k))])]
        [(and for-expose?
              (or (zero? (hash-count renames))
                  (for/and ([(k v) (in-hash renames)])
                    (not (expose? v)))))
         ;; none exposed
         null]
        [(and (not for-expose?)
              (or (zero? (hash-count renames))
                  (for/and ([(k v) (in-hash renames)])
                    (and (expose? v)
                         (eq? k (syntax-e (expose-id v)))))))
         ;; all ids, none renamed (but some exposed)
         (list lower-r)]
        [else
         ;; mixture of renames and exposes, with unmentioned imported
         (append
          (if for-expose?
              null
              (list #`(all-except #,lower-r
                                  #,@(for/list ([k (in-hash-keys renames)])
                                       k))))
          (for/list ([(k v) (in-hash renames)]
                     #:when v
                     #:when (or (not for-expose?)
                                (expose? v)))
            #`(rename #,lower-r #,(plain-id v) #,k)))]))
    (define prefix-intro (and prefix-id (make-syntax-introducer)))
    (define module? (not (list? (syntax-local-context))))
    (define reqs+defss
      (cons
       (list
        (if prefix-id
            #`(for-meta #,phase-shift
                        #,@(map prefix-intro (make-ins #f))
                        #,@(make-ins #t))
            #`(for-meta #,phase-shift
                        #,@(make-ins #f))))
       (if prefix-id
           (for/list ([phase+space (in-list r-phase+spaces)]
                      #:when (or phase-shift (eqv? 0 (phase+space-phase phase+space))))
             (define phase (phase+space-phase phase+space))
             (define space (phase+space-space phase+space))
             (define s-prefix-id (if space
                                     ((make-interned-syntax-introducer space) prefix-id)
                                     prefix-id))
             (define portal-id (if module?
                                   s-prefix-id
                                   (car (generate-temporaries (list s-prefix-id)))))
             (cons
              #`(for-meta #,(and phase phase-shift (+ phase-shift phase))
                          (portal #,portal-id ([import #,core-r #,(strip-prefix r mod-path)]
                                               #,s-prefix-id
                                               #,(prefix-intro s-prefix-id))))
              (if module?
                  null
                  (list #`(define-syntax #,s-prefix-id (make-rename-transformer (quote-syntax #,portal-id)))))))
           null)))
    ;; in a local-binding context, use `define-syntax` instead of `#%require`
    ;; for `s-prefix-id`, since a `#%require` will be lifted
    (if module?
        #`(#%require #,@(map car reqs+defss))
        #`(begin
            (#%require #,@(map car reqs+defss))
            #,@(apply append (map cdr reqs+defss))))))
