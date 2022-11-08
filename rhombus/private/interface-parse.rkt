#lang racket/base
(require enforest/syntax-local
         "introducer.rkt"
         (only-in "class-parse.rkt"
                  in-class-desc-space))

(provide (struct-out interface-desc)
         interface-desc-ref
         interface-names->interfaces
         close-interfaces-over-superinterfaces)

(struct interface-desc (id
                        super-ids
                        interface:id
                        ref-id
                        method-names  ; same as `class-desc`
                        method-vtable ; same as `class-desc`
                        method-map))  ; same as `class-desc`
(define (interface-desc-ref v) (and (interface-desc? v) v))

(define (interface-names->interfaces stxes names)
  (define intfs
    (for/list ([name (in-list names)])
      (or (syntax-local-value* (in-class-desc-space name) interface-desc-ref)
          (raise-syntax-error #f "not an interface name" stxes name))))
  ;; remove duplicates, just to make the generated class or interface description more compact
  (let loop ([ht #hasheq()] [intfs intfs])
    (cond
      [(null? intfs) null]
      [(hash-ref ht (car intfs) #f) (loop ht (cdr intfs))]
      [else (cons (car intfs) (loop (hash-set ht (car intfs) #t) (cdr intfs)))])))

(define (close-interfaces-over-superinterfaces interfaces)
  (let loop ([seen #hasheq()] [ints interfaces])
    (cond
      [(null? ints) null]
      ((hash-ref seen (car ints) #f) (loop seen (cdr ints)))
      [else
       (define intf (car ints))
       (define supers (for/list ([id (in-list (syntax->list (interface-desc-super-ids intf)))])
                        (or (syntax-local-value* (in-class-desc-space id) interface-desc-ref)
                            (error "missing interface" id))))
       (cons intf
             (loop (hash-set seen intf #t)
                   (append supers (cdr ints))))])))
