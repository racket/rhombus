#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/phase+space
                     enforest/transformer
                     "import-cover.rkt")
         "name-root-space.rkt"
         "space-in.rkt")

;; Convert a subset of `racket` require clauses to `#%require` clauses,
;; including the use of `portal` for a prefixed import

(provide (for-syntax lower-require-clause)
         expose-in
         import-dotted)

(define-syntax expose-in #f)
(define-syntax import-dotted #f)

(begin-for-syntax
  (define (lower-require-clause inner-r r mod-path prefix-id covered-ht accum?)
    (define (expose v) (vector v))
    (define (expose? v) (vector? v))
    (define (expose-id v) (vector-ref v 0))
    (define-values (r-phase+spaces syms new-covered-ht phase-shift renames revnames only-mentioned?)
      ;; A call to `extract` returns
      ;;  - phases+spaces: relevant phase+space combinations
      ;;  - syms: all names imported at relevant phase+space by the specification so far, mapped to spaces where it resides
      ;;  - covered-ht: similar info to make sure all actions are covered across multiple import conversions
      ;;  - shift: phase shift
      ;;  - renames: summarizes rename/exclude history (redundant with `syms`, but allows some improved errors and optimization)
      ;;  - revnames: more rename summary (similarly redundant with `syms`)
      ;;  - only-mentioned?: more about renames/excludes
      (let loop ([r inner-r] [outer-r r])
        (let extract ([r r] [space '#:all] [step 0])
          (define (root)
            (define all-phase+space+symss (syntax-local-module-exports (syntax->datum mod-path)))
            (define phase+space+symss
              (cond
                [(eq? space '#:all) all-phase+space+symss]
                [else (for/list ([phase+space+syms (in-list all-phase+space+symss)]
                                 #:when (eq? space (phase+space-space (car phase+space+syms))))
                        phase+space+syms)]))
            (define phase+spaces (map car phase+space+symss))
            (define syms (for*/fold ([syms #hasheq()])
                                    ([phase+space+syms (in-list phase+space+symss)]
                                     [sym (in-list (cdr phase+space+syms))])
                           (hash-set syms sym (hash-set (hash-ref syms sym #hasheq()) (phase+space-space (car phase+space+syms)) #t))))
            (values phase+spaces syms covered-ht 0 #hasheq() #hasheq() #f))
          (syntax-parse r
            #:datum-literals (rename-in only-in except-in expose-in for-meta for-label
                                        rhombus-prefix-in only-spaces-in except-spaces-in only-space-in)
            [#f (if outer-r
                    (loop outer-r #f)
                    (root))]
            [(rename-in mp [orig bind] ...)
             (define-values (phase+spaces syms covered-ht shift renames revnames only-mentioned?) (extract #'mp space (add1 step)))
             (define-values (new-renames new-revnames removed-syms new-covered-ht)
               (for/fold ([renames renames]
                          [revnames revnames]
                          [syms syms]
                          [covered-ht covered-ht])
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
                            (values (hash-set renames true-orig (if (expose? old-orig)
                                                                    (expose bind-s)
                                                                    bind-s))
                                    (hash-set (hash-remove revnames orig) bind true-orig)
                                    (hash-remove syms orig)
                                    (cover covered-ht orig step)))]
                      [else
                       (raise-syntax-error 'import "identifier to rename is excluded" orig-s)])]
                   [else
                    (when (or only-mentioned?
                              (and (not accum?)
                                   (not (or (hash-ref syms orig #f)
                                            (covered? covered-ht orig step)))))
                      (raise-syntax-error 'import "identifier to rename is not included" orig-s))
                    (values (hash-set renames orig bind-s)
                            (hash-set revnames bind orig)
                            (hash-remove syms orig)
                            (cover covered-ht orig step))])))
             ;; second pass to add renamed syms (in case a name is both removed and added)
             (define new-syms
               (for/fold ([new-syms removed-syms])
                         ([orig-s (in-list (syntax->list #'(orig ...)))]
                          [bind-s (in-list (syntax->list #'(bind ...)))])
                 (define orig (syntax-e orig-s))
                 (define bind (syntax-e bind-s))
                 (define spaces (hash-ref syms orig #f))
                 (if spaces
                     (hash-set new-syms bind spaces)
                     new-syms)))
             (values phase+spaces new-syms new-covered-ht shift new-renames new-revnames only-mentioned?)]
            [(only-in mp id ...)
             (define-values (phase+spaces syms covered-ht shift renames revnames only-mentioned?) (extract #'mp space (add1 step)))
             (define-values (new-renames new-revnames new-syms new-covered-ht)
               (for/fold ([new-renames #hasheq()]
                          [new-revnames #hasheq()]
                          [new-syms #hasheq()]
                          [covered-ht covered-ht])
                         ([id-s (in-list (syntax->list #'(id ...)))])
                 (define id (syntax-e id-s))
                 (define orig (hash-ref revnames id #f))
                 (cond
                   [orig
                    (cond
                      [(hash-ref renames orig #f)
                       => (lambda (target)
                            (values (hash-set new-renames orig target)
                                    (hash-set new-revnames id orig)
                                    (hash-set new-syms id (hash-ref syms (syntax-e target)))
                                    (cover covered-ht id step)))]
                      [else
                       (raise-syntax-error 'import "identifier to include was previously excluded" id-s)])]
                   [else
                    (when (or only-mentioned?
                              (and (not accum?)
                                   (not (or (hash-ref syms id #f)
                                            (covered? covered-ht id step)))))
                      (raise-syntax-error 'import "identifier to include was not previously included" id-s))
                    (define spaces (hash-ref syms id #f))
                    (values (hash-set new-renames id id-s)
                            (hash-set new-revnames id id)
                            (if spaces
                                (hash-set new-syms id spaces)
                                new-syms)
                            (if spaces
                                (cover covered-ht id step)
                                covered-ht))])))
             (values phase+spaces new-syms new-covered-ht shift new-renames new-revnames #t)]
            [(except-in mp id ...)
             (define-values (phase+spaces syms covered-ht shift renames revnames only-mentioned?) (extract #'mp space (add1 step)))
             (define-values (new-renames new-revnames new-syms new-covered-ht)
               (for/fold ([renames renames]
                          [revnames revnames]
                          [syms syms]
                          [covered-ht covered-ht])
                         ([id-s (in-list (syntax->list #'(id ...)))])
                 (define id (syntax-e id-s))
                 (define orig (hash-ref revnames id #f))
                 (cond
                   [orig
                    (cond
                      [(hash-ref renames orig #f)
                       (values (if only-mentioned?
                                   (hash-remove renames orig)
                                   (hash-set renames orig #f))
                               (if only-mentioned?
                                   (hash-remove revnames id)
                                   revnames)
                               (hash-remove syms orig)
                               (cover covered-ht orig step))]
                      [else
                       (raise-syntax-error 'import "identifier to exclude was previously excluded" id-s)])]
                   [else
                    (when (or only-mentioned?
                              (and (not accum?)
                                   (not (or (hash-ref syms id #f)
                                            (covered? covered-ht orig step)))))
                      (raise-syntax-error 'import "identifier to exclude was not previously included" id-s))
                    (values (hash-set renames id #f)
                            (hash-set revnames id id)
                            (hash-remove syms orig)
                            (if (hash-ref syms orig #f)
                                (cover covered-ht orig step)
                                covered-ht))])))
             (values phase+spaces new-syms new-covered-ht shift new-renames new-revnames only-mentioned?)]
            [(expose-in mp id ...)
             (define-values (phase+spaces syms covered-ht shift renames revnames only-mentioned?) (extract #'mp space (add1 step)))
             (define-values (new-renames new-revnames new-covered-ht)
               (for/fold ([renames renames]
                          [revnames revnames]
                          [new-covered-ht covered-ht])
                         ([id-s (in-list (syntax->list #'(id ...)))])
                 (define id (syntax-e id-s))
                 (define orig (hash-ref revnames id #f))
                 (cond
                   [(not orig)
                    (when (or only-mentioned?
                              (not (or accum?
                                       (hash-ref syms id #f)
                                       (covered? covered-ht id step))))
                      (raise-syntax-error 'import "identifier to expose was not previously included" id-s))
                    (values (hash-set renames id (expose id-s))
                            (hash-set revnames id id)
                            (if (hash-ref syms id #f)
                                (cover covered-ht id step)
                                covered-ht))]
                   [(hash-ref renames orig #f)
                    (values (hash-set renames orig (expose id-s))
                            revnames
                            (cover covered-ht orig step))]
                   [else
                    (raise-syntax-error 'import "identifier to expose was previously excluded" id-s)])))
             (values phase+spaces syms new-covered-ht shift new-renames new-revnames only-mentioned?)]
            [(for-meta phase mp)
             (define-values (phase+spaces syms covered-ht shift renames revnames only-mentioned?) (extract #'mp space step))
             (define new-shift (and shift (syntax-e #'phase)
                                    (+ shift (syntax-e #'phase))))
             (values phase+spaces syms covered-ht new-shift renames revnames only-mentioned?)]
            [(for-label mp)
             (define-values (phase+spaces syms covered-ht shift renames revnames only-mentioned?) (extract #'mp space step))
             (define new-shift #f)
             (values phase+spaces syms covered-ht new-shift renames revnames only-mentioned?)]
            [((~and mode (~or only-spaces-in except-spaces-in)) mp a-space ...)
             (define-values (phase+spaces syms covered-ht shift renames revnames only-mentioned?) (extract #'mp space step))
             (define the-spaces
               (for/hasheq ([a-space (in-list (syntax->list #'(a-space ...)))])
                 (values (syntax-e a-space) #t)))
             (define keep? (free-identifier=? #'mode #'only-spaces-in))
             (define new-syms
               (for/fold ([new-syms #hasheq()])
                         ([(sym spaces*) (in-hash syms)])
                 (define spaces (if (pair? spaces*) (car spaces*) spaces*))
                 (define new-spaces (for/hasheq ([space (in-hash-keys spaces)]
                                                 #:when (if keep?
                                                            (hash-ref the-spaces space #f)
                                                            (not (hash-ref the-spaces space #f))))
                                      (values space #t)))
                 (if (positive? (hash-count new-spaces))
                     (hash-set new-syms sym (cond
                                              [(equal? spaces new-spaces) spaces*]
                                              [(pair? spaces*) (cons new-spaces (cdr spaces*))]
                                              [else (cons new-spaces spaces)]))
                     new-syms)))
             (define-values (new-revnames new-renames)
               (for/fold ([new-revnames revnames]
                          [new-renames renames])
                         ([(name orig) (in-hash revnames)])
                 (if (hash-ref new-syms name)
                     (values new-revnames new-renames)
                     (values (hash-remove new-revnames name)
                             (hash-remove new-renames orig)))))
             (define new-phase+spaces
               (for/list ([phase+space (in-list phase+spaces)]
                          #:when (let ([space (phase+space-space phase+space)])
                                   (if keep?
                                       (hash-ref the-spaces space #f)
                                       (not (hash-ref the-spaces space #f)))))
                 phase+space))
             (values new-phase+spaces new-syms covered-ht shift new-renames new-revnames only-mentioned?)]
            [(only-space-in new-space mp)
             (unless (eq? space '#:all)
               (raise-syntax-error 'import "duplicate or conflicting space" #'new-space))
             (extract #'mp (syntax-e #'new-space) step)]
            [(rhombus-prefix-in mp name) (extract #'mp space step)]
            [_ (raise-syntax-error 'import
                                   "don't know how to lower"
                                   r)]))))
    (define (strip-prefix r mp)
      (let strip ([r r])
        (syntax-parse r
          #:literals (rename-in only-in except-in expose-in for-label for-meta only-space-in
                                only-spaces-in except-spaces-in)
          [#f mp]
          [((~and tag (~or rename-in only-in except-in expose-in for-label only-spaces-in except-spaces-in))
            mp . rest)
           #`(tag #,(strip #'mp) . rest)]
          [((~and tag for-meta) phase mp)
           #`(tag phase #,(strip #'mp))]
          [((~and tag only-space-in) space mp)
           #`(tag space #,(strip #'mp))]
          [((~literal rhombus-prefix-in) mp name) (strip #'mp)]
          [_ (raise-syntax-error 'import "don't know how to strip" r)])))
    (define (plain-id v) (if (expose? v) (expose-id v) v))
    (define any-space-limited?
      (for/and ([spaces* (in-hash-values syms)])
        (pair? spaces*)))
    (define space->ids
      (and any-space-limited?
           (for*/fold ([space->ids #hasheq()])
                      ([(k spaces*) (in-hash syms)]
                       [space (in-hash-keys (if (pair? spaces*)
                                                (car spaces*)
                                                spaces*))])
             (hash-set space->ids space
                       (hash-set (hash-ref space->ids space #hasheq()) k #t)))))
    (define (make-ins* syms for-expose?)
      (cond
        [only-mentioned?
         (cond
           [(for/and ([(k v) (in-hash renames)])
              (eq? k v))
            ;; only specific ids, none exposed or renamed
            (if for-expose?
                '()
                (list #'(only #,mod-path
                              #,@(for/list ([k (in-hash-keys renames)]
                                            #:when (hash-ref syms k #f))
                                   k))))]
           [else
            ;; specific ids, some exposed or renamed
            (define l
              (for/list ([(k v) (in-hash renames)]
                         #:when (and (or (not for-expose?)
                                         (expose? v))
                                     (hash-ref syms (syntax-e (plain-id v)) #f)))
                #`(rename #,mod-path #,(plain-id v) #,k)))
            (if (null? l)
                ;; don't lose track of the module completely
                (list #`(only #,mod-path))
                l)])]
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
         (list mod-path)]
        [else
         ;; mixture of renames and exposes, with unmentioned imported
         (append
          (if for-expose?
              null
              (list #`(all-except #,mod-path
                                  ;; things in `renames` are renamed or excluded
                                  #,@(for/list ([k (in-hash-keys renames)])
                                       k))))
          (for/list ([(k v) (in-hash renames)]
                     #:when v ;; #f means excluded
                     #:when (and (or (not for-expose?)
                                     (expose? v))
                                 (hash-ref syms (syntax-e (plain-id v)) #f)))
            #`(rename #,mod-path #,(plain-id v) #,k)))]))
    (define (make-ins for-expose?)
      (if any-space-limited?
          (for/list ([(space ids) (in-hash space->ids)])
            #`(just-space #,space #,@(make-ins* ids for-expose?)))
          (make-ins* syms for-expose?)))
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
           (for/list ([phase (in-hash-keys
                              (for/hasheqv ([phase+space (in-list r-phase+spaces)]
                                            #:when (or phase-shift (eqv? 0 (phase+space-phase phase+space))))
                                (values (phase+space-phase phase+space) #t)))])
             (define s-prefix-id prefix-id)
             (define portal-id (in-name-root-space s-prefix-id))
             (list
              #`(for-meta #,(and phase phase-shift (+ phase-shift phase))
                          (portal #,portal-id ([import #,mod-path
                                                       #,(strip-prefix r mod-path)
                                                       #,(datum->syntax mod-path 'mod-ctx)]
                                               #,s-prefix-id
                                               #,(prefix-intro s-prefix-id))))))
           null)))
    (values
     (if (null? r-phase+spaces)
         ;; don't lose track of the dependency:
         #`(#%require (for-meta #,phase-shift) (only #,mod-path))
         (if module?
             #`(#%require #,@(map car reqs+defss))
             #`(begin
                 (#%require #,@(map car reqs+defss))
                 #,@(apply append (map cdr reqs+defss)))))
     new-covered-ht)))
