#lang racket/base
(require enforest/syntax-local
         "introducer.rkt"
         (only-in "class-parse.rkt"
                  in-class-desc-space
                  objects-desc
                  objects-desc-interface-ids
                  objects-desc-flags)
         (for-template "expression.rkt"))

(provide (struct-out interface-desc)
         (struct-out interface-internal-desc)
         interface-desc-ref
         interface-names->interfaces
         interface-set-diff
         close-interfaces-over-superinterfaces
         interface-names->quoted-list)

(struct interface-desc objects-desc
  ;; `flags` from `objects-desc` can include 'veneer
  (id
   internal-id
   prop:id
   prop:internal-internal-id
   ref-id
   custom-annotation?
   indirect-call-method-id)) ; same as `class-desc`
(struct interface-internal-desc interface-desc (private-methods      ; (list symbol ...)
                                                private-properties)) ; (list symbol ...)

(define (interface-desc-ref v) (and (interface-desc? v) v))
(define (interface-noninternal-desc-ref v) (and (interface-desc? v)
                                                (not (interface-internal-desc? v))
                                                v))

(define (interface-names->interfaces stxes names
                                     #:results [results (lambda (all pruned) pruned)]
                                     #:for-veneer? [for-veneer? #f])
  (define intfs
    (for/list ([name (in-list names)])
      (define intf
        (or (syntax-local-value* (in-class-desc-space name) interface-noninternal-desc-ref)
            (raise-syntax-error #f "not an interface name" stxes name)))
      (when (and for-veneer?
                 (not (memq 'veneer (objects-desc-flags intf))))
        (raise-syntax-error #f "interface cannot be implemented by a veneer" stxes name))
      intf))
  (results
   intfs
   ;; remove duplicates, just to make the generated class or interface description more compact
   (let loop ([ht #hasheq()] [intfs intfs])
     (cond
       [(null? intfs) null]
       [(hash-ref ht (car intfs) #f) (loop ht (cdr intfs))]
       [else (cons (car intfs) (loop (hash-set ht (car intfs) #t) (cdr intfs)))]))))

(define (interface-set-diff l1 l2)
  (define s2 (for/hasheq ([intf (in-list l2)]) (values intf #t)))
  (for/hasheq ([intf (in-list l1)]
               #:unless (hash-ref s2 intf #f))
    (values intf #t)))

(define (close-interfaces-over-superinterfaces interfaces private-interfaces)
  (let loop ([seen #hasheq()]
             [priv-seen private-interfaces]
             [int+priv?s (for/list ([intf (in-list interfaces)])
                           (cons intf (hash-ref private-interfaces intf #f)))])
    (cond
      [(null? int+priv?s)
       (append (for/list ([intf (in-hash-keys seen)])
                 intf)
               ;; for privately implemented interfaces, return the internal
               ;; interface, if it exists; otherwise, if there's an internal-internal
               ;; property, synthesize by shifting that internal property id into place:
               (for/list ([intf (in-hash-keys priv-seen)]
                          #:do [(define int-id (interface-desc-internal-id intf))]
                          #:when (or int-id
                                     (interface-desc-prop:internal-internal-id intf)))
                 (cond
                   [int-id
                    (or (syntax-local-value* (in-class-desc-space int-id) interface-desc-ref)
                        (raise-syntax-error #f "could not find internal interface" int-id))]
                   [else
                    (struct-copy interface-desc intf
                                 [prop:id (interface-desc-prop:internal-internal-id intf)])])))]
      [(hash-ref seen (caar int+priv?s) #f)
       (loop seen priv-seen (cdr int+priv?s))]
      [(and (hash-ref priv-seen (caar int+priv?s) #f)
            (cdar int+priv?s))
       (loop seen priv-seen (cdr int+priv?s))]
      [else
       (define intf+priv? (car int+priv?s))
       (define intf (car intf+priv?))
       (define priv? (cdr intf+priv?))
       (define supers (for/list ([id (in-list (syntax->list (objects-desc-interface-ids intf)))])
                        (cons (or (syntax-local-value* (in-class-desc-space id) interface-desc-ref)
                                  (error "missing interface" id))
                              priv?)))
       (if priv?
           (loop seen
                 (hash-set priv-seen intf #t)
                 (append supers (cdr int+priv?s)))
           (loop (hash-set seen intf #t)
                 (hash-remove priv-seen intf)
                 (append supers (cdr int+priv?s))))])))

(define (interface-names->quoted-list interface-names interfaces only-ht mode)
  (let loop ([seen #hasheq()]
             [names interface-names]
             [intfs interfaces])
    (cond
      [(null? names) '()]
      [(or (hash-ref seen (car intfs) #f)
           ((if (eq? mode 'public) values not) (hash-ref only-ht (car intfs) #f)))
       (loop seen (cdr names) (cdr intfs))]
      [else
       (cons (car names) (loop (hash-set seen (car intfs) #t) (cdr names) (cdr intfs)))])))
