#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/symbol
                     (prefix-in enforest: enforest/name-root)
                     enforest/syntax-local
                     shrubbery/property
                     "srcloc.rkt"))

;; convert a hierachical layer implemented as portal syntax to a name-root

(provide (for-syntax name-root-ref
                     make-name-root-ref
                     name-root-ref-root
                     portal-syntax->lookup
                     replace-head-dotted-name
                     import-root-ref
                     extensible-name-root?))

(define-for-syntax (make-name-root-ref in-space binding-ref)
  (lambda (v)
    (define (make self-id get)
      (enforest:name-root
       (lambda (stxes)
         (let loop ([stxes stxes]
                    [gets
                     ;; reverse order search path: (cons get prefix)
                     (list
                      (cons get #f)
                      (cons #f (syntax-parse stxes
                                 [(form-id . _) #'form-id])))])
           (define (build-name prefix field-id)
             (datum->syntax prefix
                            (string->symbol
                             (string-append (symbol->immutable-string (syntax-e prefix))
                                            "."
                                            (symbol->immutable-string (syntax-e field-id))))
                            field-id
                            field-id))
           (define (next form-id field-id what tail)
             (define binding-end? (and binding-ref
                                       (syntax-parse tail
                                         #:datum-literals (op parens |.|)
                                         [((op |.|) . _) #f]
                                         [_ #t])))
             (define id
               (or (for/or ([get+prefix (in-list (reverse gets))])
                     (define get (car get+prefix))
                     (define prefix (cdr get+prefix))
                     (cond
                       [(not get)
                        (define name (build-name prefix field-id))
                        (and (identifier-binding (in-space name))
                             (relocate-field form-id field-id name))]
                       [else
                        (define sub-id (if prefix
                                           (build-name prefix field-id)
                                           field-id))
                        (let ([id (get #f what sub-id)])
                          (and id
                               (or (not binding-end?)
                                   (syntax-local-value* (in-space id)
                                                        (lambda (v)
                                                          (name-root-ref-root v binding-ref))))
                               (relocate-field form-id field-id id)))]))
                   (if binding-end?
                       (relocate-field form-id field-id (build-name (cdar (reverse gets)) field-id))
                       ;; try again with the shallowest to report an error
                       (let ([get (caar gets)])
                         (get form-id what field-id)))))
             ;; keep looking at dots?
             (define more-dots?
               (syntax-parse tail
                 #:datum-literals (op parens group |.|)
                 [((op |.|) _:identifier . _) #t]
                 [((op |.|) (parens (group target (op _))) . tail) #t]
                 [_ #f]))
             (define v (and more-dots?
                            (syntax-local-value* (in-space id) (lambda (v) (and (portal-syntax? v) v)))))
             (cond
               [v
                (portal-syntax->lookup (portal-syntax-content v)
                                       (lambda (self-id next-get)
                                         (loop (cons id tail)
                                               (cons
                                                (cons next-get #f)
                                                (for/list ([get+prefix (in-list gets)])
                                                  (define get (car get+prefix))
                                                  (define prefix (cdr get+prefix))
                                                  (cons get (if prefix
                                                                (build-name prefix field-id)
                                                                field-id)))))))]
               [else
                (values id tail)]))
           (syntax-parse stxes
             #:datum-literals (op parens group |.|)
             [(form-id (op |.|) field:identifier . tail)
              (next #'form-id #'field "identifier" #'tail)]
             [(form-id (op |.|) (parens (group (~and target (op field)))) . tail)
              (next #'form-id #'field "operator" #'tail)]
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

(define-for-syntax name-root-ref (make-name-root-ref (lambda (x) x) #f))

(define-for-syntax (name-root-ref-root v ref)
  (or (and
       (portal-syntax? v)
       (portal-syntax->lookup (portal-syntax-content v)
                              (lambda (self-id get)
                                (define id (get #f #f #'#f))
                                (and id
                                     (syntax-local-value* id ref)))))
      (ref v)))

(define-for-syntax (portal-syntax->lookup portal-stx make)
  (syntax-parse portal-stx
    #:datum-literals (import map)
    [([import _ _ _] pre-ctx-s ctx-s)
     (define pre-ctx #'pre-ctx-s)
     (define ctx #'ctx-s)
     (make #f
           (lambda (who-stx what name)
             (cond
               [(syntax-e name)
                (define id (datum->syntax ctx
                                          (syntax-e name)
                                          name
                                          name))
                (define pre-id (datum->syntax pre-ctx (syntax-e name)))
                (cond
                  [(identifier-distinct-binding id pre-id)
                   id]
                  [who-stx
                   (raise-syntax-error #f
                                       (format "no such imported ~a" what)
                                       name)]
                  [else #f])]
               [else #f])))]
    [(map self-id [key val] ...)
     (define keys (syntax->list #'(key ...)))
     (define vals (syntax->list #'(val ...)))
     (make #'self-id
           (lambda (who-stx what name)
             (or (for/or ([key (in-list keys)]
                          [val (in-list vals)])
                   (and (eq? (syntax-e key) (syntax-e name))
                        val))
                 (and who-stx
                      (raise-syntax-error #f
                                          (format "~a not provided by ~a"
                                                  what
                                                  (syntax-e who-stx))
                                          name)))))]
    [_ #f]))

(define-for-syntax (relocate-field root-id field-id new-field-id)
  (syntax-property (datum->syntax new-field-id
                                  (syntax-e new-field-id)
                                  (span-srcloc root-id field-id)
                                  field-id)
                   'rhombus-dotted-name
                   (string->symbol
                    (format "~a.~a"
                            (or (syntax-property root-id 'syntax-error-name)
                                (syntax-e root-id))
                            (syntax-e field-id)))))

(define-for-syntax (replace-head-dotted-name stx)
  (define head (car (syntax-e stx)))
  (define name (syntax-property head 'rhombus-dotted-name))
  (cond
    [name
     (datum->syntax stx
                    (cons (syntax-raw-property (datum->syntax head name head head)
                                               (symbol->string name))
                          (cdr (syntax-e stx)))
                    stx
                    stx)]
    [else stx]))
                             
;; Gets information on a name ref that can be used with `import`
(define-for-syntax (import-root-ref v)
  (and
   (portal-syntax? v)
   (portal-syntax->import (portal-syntax-content v))))

(define-for-syntax (portal-syntax->import portal-stx)
  (syntax-parse portal-stx
    #:datum-literals (map import)
    [([import mod-path parsed-r mod-ctx] orig-s ctx-s)
     #`(parsed #,(datum->syntax #'ctx-s (syntax-e #'mod-path)) #,((make-syntax-delta-introducer
                                                                   #'mod-ctx
                                                                   (syntax-local-introduce (datum->syntax #f 'empty)))
                                                                  #'parsed-r
                                                                  'remove))]
    [(map _ [key val] ...)
     portal-stx]
    [_ #f]))

(define-for-syntax (extensible-name-root? ids)
  (let loop ([ids ids] [portal-stx #f] [prev-who #f])
    (define id
      (cond
        [(not portal-stx) (car ids)]
        [else
         (portal-syntax->lookup portal-stx
                                (lambda (self-id get)
                                  (define id (get #f #f (car ids)))
                                  (and id
                                       (relocate-field prev-who (car ids) id))))]))
    (define v
      (and id
           (syntax-local-value* id (lambda (v)
                                     (and (portal-syntax? v)
                                          v)))))
    (and v
         (cond
           [(null? (cdr ids))
            ;; must be `map` portal syntax to allow extension:
            (syntax-parse (portal-syntax-content v)
              #:datum-literals (map)
              [(map . _) #t]
              [_ #f])]
           [else
            (loop (cdr ids) (portal-syntax-content v) id)]))))
