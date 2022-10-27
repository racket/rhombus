#lang racket/base
(require enforest/syntax-local
         "introducer.rkt"
         (only-in "class-parse.rkt"
                  in-class-desc-space)
         (for-template "name-root-ref.rkt"
                       "expression.rkt"))

(provide (struct-out interface-desc)
         interface-desc-ref
         interface-names->interfaces
         interface-set-diff
         close-interfaces-over-superinterfaces)

(struct interface-desc (id
                        internal-id
                        super-ids
                        prop:id
                        ref-id
                        method-shapes   ; same as `class-desc`
                        method-vtable   ; same as `class-desc`
                        method-map      ; same as `class-desc`
                        method-result)) ; same as `class-desc`
(define (interface-desc-ref v) (and (interface-desc? v) v))

(define (interface-names->interfaces stxes names)
  (define intfs
    (for/list ([name (in-list names)])
      (or (syntax-local-value* (in-class-desc-space name)
                               (lambda (v)
                                 (name-root-ref-root v interface-desc-ref)))
          (raise-syntax-error #f "not an interface name" stxes name))))
  ;; remove duplicates, just to make the generated class or interface description more compact
  (let loop ([ht #hasheq()] [intfs intfs])
    (cond
      [(null? intfs) null]
      [(hash-ref ht (car intfs) #f) (loop ht (cdr intfs))]
      [else (cons (car intfs) (loop (hash-set ht (car intfs) #t) (cdr intfs)))])))

(define (interface-set-diff l1 l2)
  (define s2 (for/hasheq ([intf (in-list l2)]) (values intf #t)))
  (for/hasheq ([intf (in-list l1)]
               #:unless (hash-ref s2 intf #f))
    (values intf #t)))

(define (close-interfaces-over-superinterfaces interfaces private-interfaces)
  (let loop ([seen #hasheq()]
             [priv-seen #hasheq()]
             [int+priv?s (for/list ([intf (in-list interfaces)])
                           (cons intf (hash-ref private-interfaces intf #f)))])
    (cond
      [(null? int+priv?s)
       (append (for/list ([intf (in-hash-keys seen)])
                 intf)
               ;; for privately implemented interfaces, return the internal
               ;; interface, if it exists
               (for/list ([intf (in-hash-keys priv-seen)]
                          #:do [(define int-id (interface-desc-internal-id intf))]
                          #:when int-id)
                 (or (syntax-local-value* (in-class-desc-space int-id) interface-desc-ref)
                     (raise-syntax-error #f "could not find internal interface" int-id))))]
      [(hash-ref seen (caar int+priv?s) #f)
       (loop seen priv-seen (cdr int+priv?s))]
      [(and (hash-ref priv-seen (caar int+priv?s) #f)
            (cdar int+priv?s))
       (loop seen priv-seen (cdr int+priv?s))]
      [else
       (define intf+priv? (car int+priv?s))
       (define intf (car intf+priv?))
       (define priv? (cdr intf+priv?))
       (define supers (for/list ([id (in-list (syntax->list (interface-desc-super-ids intf)))])
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
