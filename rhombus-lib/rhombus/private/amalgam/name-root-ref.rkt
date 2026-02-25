#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol
                     (prefix-in enforest: enforest/name-root)
                     enforest/syntax-local
                     enforest/transformer
                     shrubbery/property
                     shrubbery/print
                     "srcloc.rkt"
                     "id-binding.rkt")
         ;; to support `Syntax.same_binding` and `Syntax.meta_value`, this
         ;; module is instantiated in phase 0 as well as 1, so keep the
         ;; imports suitably limited
         "name-root-space.rkt")

;; convert a hierarchical layer implemented as portal syntax to a name-root

(provide (for-syntax name-root-ref
                     name-root-ref/maybe
                     name-root-ref/or-expr
                     make-name-root-ref
                     import-root-ref
                     extensible-name-root
                     portal-syntax->lookup
                     portal-syntax->import
                     portal-syntax->extends
                     name-root-all-out
                     dotted-binding-id))

(begin-for-syntax
  (define (build-name prefix field-id #:ctx [ctx prefix])
    (syntax-property
     (datum->syntax ctx
                    (string->symbol
                     (string-append (symbol->immutable-string (syntax-e prefix))
                                    "."
                                    (symbol->immutable-string (syntax-e field-id))))
                    field-id
                    field-id)
     'origin
     (syntax-local-introduce (in-name-root-space prefix)))))

;; * `binding-ref` as non-#f means that we're parsing a binding, so we
;;    want to follow namespaces, but a tail non-bound name is one to
;;    be bound (so don't complain if it's not bound). This path is
;;    also used for exports.
;; * `non-portal-ref` as non-#f is similar. but for imports; we want to build
;;    a "dotted" form
(define-for-syntax (make-name-root-ref #:binding-ref [binding-ref #f] ;; see above
                                       #:non-portal-ref [non-portal-ref #f] ;; see above
                                       #:binding-extension-combine [binding-extension-combine (lambda (prefix field-id id) id)]
                                       #:dot-name-construction [dot-name-construction (lambda (names id) id)]
                                       #:quiet-fail? [quiet-fail? #f]
                                       #:fallback-to-expr? [fallback-to-expr? #f])
  (lambda (v)
    (define (make self-id get)
      (enforest:name-root
       (lambda (in-space stxes)
         ;; This search loop lets us traverse A.B.C.D to
         ;; keep going as long as there are namespace bindings,
         ;; but back up if we don't find a binding in the space
         ;; indicated by `in-space`
         (define head
           (syntax-parse stxes
             [(form-id . _) #'form-id]))
         (let loop ([stxes stxes]
                    [gets
                     ;; reverse order search path: (cons get prefix)
                     (list
                      (cons get #f)
                      (cons #f head))]
                    [rev-dot-names (list head)])
           (define (next form-id field-id field-op-parens what tail)
             (define binding-end? (and binding-ref
                                       (syntax-parse tail
                                         #:datum-literals (op parens |.|)
                                         [((op |.|) . _) #f]
                                         [_ #t])))
             (define (get-id in-id-space ns? fail-ok?)
               (or (for/or ([get+prefix (in-list (reverse gets))])
                     (define get (car get+prefix))
                     (define prefix (cdr get+prefix))
                     (cond
                       [(not get)
                        (define name (build-name prefix field-id))
                        (and (identifier-binding* (in-id-space name))
                             (dot-name-construction
                              (reverse (cons field-id rev-dot-names))
                              (relocate-field form-id field-id name field-op-parens)))]
                       [else
                        (define sub-id (if prefix
                                           (build-name prefix field-id)
                                           field-id))
                        (let ([id (get #f what sub-id in-id-space fallback-to-expr?)])
                          (and id
                               (or ns?
                                   (not binding-end?)
                                   (syntax-local-value* (in-id-space id) binding-ref))
                               (dot-name-construction
                                (reverse (cons field-id rev-dot-names))
                                (relocate-field form-id field-id id field-op-parens))))]))
                   (cond
                     [fail-ok? #f]
                     [(or binding-end?
                          quiet-fail?)
                      (let ([prefix (cdar (reverse gets))])
                        (dot-name-construction
                         (reverse (cons field-id rev-dot-names))
                         (binding-extension-combine
                          (in-name-root-space prefix)
                          field-id
                          (relocate-field form-id
                                          field-id
                                          (build-name prefix field-id #:ctx field-id)
                                          field-op-parens))))]
                     [else
                      ;; try again with the shallowest to report an error
                      (let ([get (caar gets)])
                        (get form-id what field-id in-id-space fallback-to-expr?))])))
             ;; keep looking at dots?
             (define more-dots?
               (syntax-parse tail
                 #:datum-literals (op parens group |.|)
                 [((op |.|) _:identifier . _) #t]
                 [((op |.|) (parens (group target (op _))) . tail) #t]
                 [_ #f]))
             (define ns-id (and more-dots? (get-id in-name-root-space #t #t)))
             (define v (and (or more-dots?
                                non-portal-ref)
                            ns-id
                            (syntax-local-value* (in-name-root-space ns-id) (lambda (v) (and (portal-syntax? v) v)))))
             (cond
               [v
                (portal-syntax->lookup (portal-syntax-content v)
                                       (lambda (self-id next-get)
                                         (if more-dots?
                                             (loop (cons ns-id tail)
                                                   (cons
                                                    (cons next-get #f)
                                                    (for/list ([get+prefix (in-list gets)])
                                                      (define get (car get+prefix))
                                                      (define prefix (cdr get+prefix))
                                                      (cons get (if prefix
                                                                    (build-name prefix field-id)
                                                                    field-id))))
                                                   (cons field-id rev-dot-names))
                                             (values self-id tail))))]
               [non-portal-ref
                (non-portal-ref form-id field-id tail)]
               [else
                (define id (get-id in-space #f #f))
                (values id tail)]))
           (syntax-parse stxes
             #:datum-literals (op parens group |.|)
             [(form-id (op |.|) field:identifier . tail)
              (next #'form-id #'field #f "identifier" #'tail)]
             [(form-id (op |.|) (~and op-parens (parens (group (op field)))) . tail)
              (next #'form-id #'field #'op-parens "operator" #'tail)]
             [(form-id (op (~and dot |.|)) . tail)
              (raise-syntax-error #f
                                  "expected an identifier or parentheses after dot"
                                  #'dot)]
             [(form-id . tail)
              (raise-syntax-error #f
                                  "expected a dot after name"
                                  #'form-id)])))))
    (or
     (enforest:name-root-ref v)
     (and
      (portal-syntax? v)
      (portal-syntax->lookup (portal-syntax-content v) make)))))

(define-for-syntax name-root-ref (make-name-root-ref))
(define-for-syntax name-root-ref/maybe (make-name-root-ref #:quiet-fail? #t))
(define-for-syntax name-root-ref/or-expr (make-name-root-ref #:fallback-to-expr? #t))

(define-for-syntax (portal-syntax->lookup portal-stx make [phase 'default])
  (syntax-parse portal-stx
    #:datum-literals (import nspace)
    [([import _ _ _] pre-ctx-s ctx-s)
     (define pre-ctx #'pre-ctx-s)
     (define ctx #'ctx-s)
     (make #f
           (lambda (who-stx what name in-space [fallback-to-expr? #f] [field-phase 0])
             (cond
               [(syntax-e name)
                (define id (datum->syntax ctx
                                          (syntax-e name)
                                          name
                                          name))
                (define pre-id (datum->syntax pre-ctx (syntax-e name)))
                (define (shift id) (if (eqv? field-phase 0) id (syntax-shift-phase-level (- field-phase))))
                (cond
                  [(identifier-distinct-binding* (shift (in-space id)) (shift (in-space pre-id))
                                                 (if (eq? phase 'default)
                                                     (syntax-local-phase-level)
                                                     phase))
                   id]
                  [(identifier-distinct-binding* (shift (in-name-root-space id)) (shift (in-name-root-space pre-id))
                                                 (if (eq? phase 'default)
                                                     (syntax-local-phase-level)
                                                     phase))
                   id]
                  [who-stx
                   (raise-syntax-error #f
                                       (format "no such imported ~a" what)
                                       name)]
                  [else #f])]
               [else #f])))]
    [(nspace self-id _ [key val . rule] ...)
     (define keys (syntax->list #'(key ...)))
     (define vals (syntax->list #'(val ...)))
     (define rules (syntax->list #'(rule ...)))
     (make #'self-id
           (lambda (who-stx what name in-space [fallback-to-expr? #f] [field-phase 0])
             (or (for/or ([key (in-list keys)]
                          [val (in-list vals)]
                          [rule (in-list rules)])
                   (and (eq? (syntax-e key) (syntax-e name))
                        (rule->identifier in-space rule val)))
                 (and fallback-to-expr?
                      (for/or ([key (in-list keys)]
                               [val (in-list vals)]
                               [rule (in-list rules)])
                        (and (eq? (syntax-e key) (syntax-e name))
                             (rule->identifier (lambda (x) x) rule val))))
                 (and who-stx
                      (raise-syntax-error #f
                                          (format "~a not provided by ~a"
                                                  what
                                                  (syntax-e who-stx))
                                          name)))))]
    [_ #f]))

(define-for-syntax (rule->identifier in-space rule val-id)
  (define (target-space? sp x)
    (bound-identifier=? (in-space x)
                        (if sp
                            ((make-interned-syntax-introducer sp) x)
                            x)))
  (let loop ([rule rule])
    (syntax-parse rule
      [() val-id]
      [(#:space ([space space-id] ...) . rest-rule)
       (define x (datum->syntax #f 'x))
       (or (for/or ([sp-stx (in-list (syntax->list #'(space ...)))]
                    [space-id (in-list (syntax->list #'(space-id ...)))])
             (and (target-space? (syntax-e sp-stx) x)
                  (in-space space-id)))
           (loop #'rest-rule))]
      [((~and mode (~or* #:only #:except)) space ...)
       (define x (datum->syntax #f 'x))
       (define match?
         (for/or ([sp-stx (in-list (syntax->list #'(space ...)))])
           (target-space? (syntax-e sp-stx) x)))
       (and (if (eq? (syntax-e #'mode) '#:only)
                 match?
                (not match?))
            val-id)])))

(define-for-syntax (portal-syntax->extends portal-stx)
  (syntax-parse portal-stx
    #:datum-literals (import nspace)
    [(nspace _ extends . _) #'extends]
    [_ #f]))

(define-for-syntax (relocate-field root-id field-id new-field-id field-op-parens)
  (define name
    (datum-intern-literal
     (format "~a.~a"
             (shrubbery-syntax->string root-id)
             (shrubbery-syntax->string (or field-op-parens field-id)))))
  (define op-parens-head (and field-op-parens (car (syntax-e field-op-parens))))
  (syntax-property
   (syntax-raw-property
    (datum->syntax new-field-id
                   (syntax-e new-field-id)
                   (span-srcloc root-id (or op-parens-head field-id))
                   (if op-parens-head
                       (syntax-raw-suffix-property
                        field-id
                        (syntax-raw-suffix-property op-parens-head))
                       field-id))
    name)
   'origin
   (let ([root (syntax-local-introduce (in-name-root-space root-id))])
     (if (syntax-original? (syntax-local-introduce field-id))
         ;; enable arrows, etc., from `new-field-id` based on its binding
         (cons (syntax-property (datum->syntax new-field-id
                                               (syntax-e new-field-id)
                                               field-id
                                               field-id)
                                ;; `new-field-id` is non-original, since it's
                                ;; introduced by expansion, and it may have other
                                ;; scopes from its definition site:
                                'original-for-check-syntax
                                #t)
               root)
         root))))

(define-for-syntax (name-root-all-out ns-id name space-syms)
  (define v (syntax-local-value* (in-name-root-space ns-id)
                                 (lambda (v) (and (portal-syntax? v) v))))
  (if (not v)
      null
      (portal-syntax->lookup (portal-syntax-content v)
                             (lambda (self-id get)
                               (for/list ([space-sym (in-list space-syms)]
                                          #:do [(define in-space
                                                  (if space-sym
                                                      (make-interned-syntax-introducer space-sym)
                                                      (lambda (x) x)))
                                                (define id (get #f #f name in-space #f))]
                                          #:when (and id
                                                      (if space-sym
                                                          (identifier-distinct-binding* (in-space id 'add)
                                                                                        (in-space id 'remove))
                                                          (identifier-binding* id))))
                                 (cons id space-sym))))))

;; Gets information on a name ref that can be used with `import`
(define-for-syntax (import-root-ref v)
  (and
   (portal-syntax? v)
   (portal-syntax->import (portal-syntax-content v))))

(define-for-syntax (portal-syntax->import portal-stx)
  (syntax-parse portal-stx
    #:datum-literals (nspace import)
    [([import mod-path parsed-r mod-ctx] orig-s ctx-s)
     #`(parsed #,(datum->syntax #'ctx-s (syntax-e #'mod-path)) #,((make-syntax-delta-introducer
                                                                   #'mod-ctx
                                                                   (syntax-local-introduce (datum->syntax #f 'empty)))
                                                                  #'parsed-r
                                                                  'remove))]
    [(nspace . _)
     portal-stx]
    [_ #f]))

;; returns the id of an extension root, the first one found from the
;; possibilities in `ids`
(define-for-syntax (extensible-name-root ids)
  (let loop ([ids ids] [portal-stx #f] [prev-who #f])
    (define id
      (cond
        [(not portal-stx) (car ids)]
        [else
         (portal-syntax->lookup portal-stx
                                (lambda (self-id get)
                                  (define id (get #f #f (car ids) in-name-root-space #f))
                                  (and id
                                       (relocate-field prev-who (car ids) id #f))))]))
    (define v
      (and id
           (syntax-local-value* (in-name-root-space id) (lambda (v)
                                                          (and (portal-syntax? v)
                                                               v)))))
    (or (and v
             (cond
               [(null? (cdr ids))
                ;; must be `map` portal syntax to allow extension:
                (syntax-parse (portal-syntax-content v)
                  #:datum-literals (nspace)
                  [(nspace . _) #t]
                  [_ #f])]
               [else
                (or (loop (cdr ids) (portal-syntax-content v) id)
                    (loop (cons (build-name (car ids) (cadr ids)) (cddr ids)) portal-stx prev-who))])
             (in-name-root-space id))
        (and (pair? (cdr ids))
             (loop (cons (build-name (car ids) (cadr ids)) (cddr ids)) portal-stx prev-who)))))
  
;; Similar to a `make-name-root-ref` search, but more direct where
;; all identifiers in a sequence must be used, and also similar to
;; `extensible-name-root`, but looking for the binding in a namespace
(define-for-syntax (dotted-binding-id ids space-sym)
  (define in-space
    (if space-sym
        (make-interned-syntax-introducer space-sym)
        (lambda (x [mode #f]) x)))
  (let loop ([ids ids] [portal-stx #f] [prev-who #f])
    (define id
      (cond
        [(not portal-stx) (car ids)]
        [else
         (portal-syntax->lookup portal-stx
                                (lambda (self-id get)
                                  (define id (get #f #f (car ids) (if (null? (cdr ids))
                                                                      (lambda (id) (in-space id 'add))
                                                                      in-name-root-space)
                                                  #f))
                                  (and id
                                       (relocate-field prev-who (car ids) id #f))))]))
    (cond
      [(not id) #f]
      [(null? (cdr ids))
       (and (if space-sym
                (identifier-distinct-binding* (in-space id 'add) (in-space id 'remove))
                (identifier-binding* id))
            (in-space id 'remove))]
      [else
       (define v
         (and id
              (syntax-local-value* (in-name-root-space id) (lambda (v)
                                                             (and (portal-syntax? v)
                                                                  v)))))
       (and v
            (or (loop (cdr ids) (portal-syntax-content v) id)
                (loop (cons (build-name (car ids) (cadr ids)) (cddr ids)) portal-stx prev-who)))])))
