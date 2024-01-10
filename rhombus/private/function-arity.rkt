#lang racket/base
(require (for-syntax racket/base
                     racket/keyword
                     racket/symbol
                     shrubbery/property
                     "statically-str.rkt"
                     "hash-set.rkt"))

(provide (for-syntax summarize-arity
                     shift-arity
                     union-arity-summaries
                     check-arity))

(define-for-syntax (summarize-arity kws defaults rest? kw-rest?)
  (define (syntax->list/maybe stx)
    (if (syntax? stx) (syntax->list stx) stx))
  (define (make-arity bit mask allowed-kws required-kws)
    (define (hash->list/keyword kws)
      (sort (hash-keys kws) keyword<?))
    (define a (bitwise-ior mask
                           (if rest?
                               (bitwise-xor -1 (sub1 bit))
                               bit)))
    (cond
      [kw-rest? (list a (hash->list/keyword required-kws) #f)]
      [(eqv? (hash-count allowed-kws) 0) a]
      [else (list a (hash->list/keyword required-kws) (hash->list/keyword allowed-kws))]))
  (for/fold ([bit 1]
             [mask 0]
             [allowed-kws #hasheq()]
             [required-kws #hasheq()]
             #:result (make-arity bit mask allowed-kws required-kws))
            ([kw (in-list (syntax->list/maybe kws))]
             [default (in-list (syntax->list/maybe defaults))])
    (cond
      [(syntax-e kw)
       (values bit
               mask
               (hash-set allowed-kws (syntax-e kw) #t)
               (if (syntax-e default)
                   required-kws
                   (hash-set required-kws (syntax-e kw) #t)))]
      [else
       (values (arithmetic-shift bit 1)
               (if (syntax-e default)
                   (bitwise-ior mask bit)
                   mask)
               allowed-kws
               required-kws)])))

(define-for-syntax (shift-arity a shift)
  (if (pair? a)
      (list (arithmetic-shift (car a) shift) (cadr a) (caddr a))
      (arithmetic-shift a shift)))

(define-for-syntax (union-arity-summaries as)
  (cond
    [(null? as) #f]
    [(null? (cdr as)) (car as)]
    [else
     (define (normalize a)
       (if (pair? a)
           (list (car a) (list->set (cadr a)) (and (caddr a) (list->set (caddr a))))
           (list a #hasheq() #hasheq())))
     (define norm-a
       (for/fold ([new-a (normalize (car as))]) ([a (in-list (cdr as))])
         (let ([a (normalize a)])
           (list (bitwise-ior (car new-a) (car a))
                 (set-intersect (cadr new-a) (cadr a))
                 (and (caddr new-a) (caddr a) (set-union (caddr new-a) (caddr a)))))))
     (define required-kws (sort (set->list (cadr norm-a)) keyword<?))
     (define allowed-kws (and (caddr norm-a) (sort (set->list (caddr norm-a)) keyword<?)))
     (if (and (null? required-kws)
              (null? allowed-kws))
         (car norm-a)
         (list (car norm-a) required-kws allowed-kws))]))

(define-for-syntax (check-arity stx fallback-stx a n kws rsts kwrsts kind)
  (define orig-needed (if (pair? a)
                          (for/hasheq ([kw (in-list (cadr a))])
                            (values kw #t))
                          #hasheq()))
  (define (error-stx) (or stx (let ([s (syntax-raw-property fallback-stx)])
                                (if (string? s)
                                    (datum->syntax #f (string->symbol s) fallback-stx)
                                    fallback-stx))))
  (let loop ([kws kws] [n n] [needed-kws #f] [allowed-kws #f])
    (cond
      [(null? kws)
       (when (zero? (bitwise-and (if rsts
                                     (if (zero? n)
                                         -1
                                         (bitwise-not (sub1 (arithmetic-shift 1 (sub1 n)))))
                                     (arithmetic-shift 1 n))
                                 (if (pair? a) (car a) a)))
         (raise-syntax-error #f
                             (case kind
                               [(property)
                                (string-append "property does not support assignment" statically-str)]
                               [else
                                (string-append "wrong number of "
                                               (if needed-kws "by-position " "")
                                               "arguments in " (symbol->immutable-string kind) " call"
                                               statically-str)])
                             (error-stx)))
       (unless kwrsts
         (define needed (or needed-kws orig-needed))
         (when (and needed ((hash-count needed) . > . 0))
           (raise-syntax-error #f
                               (string-append "missing keyword argument in " (symbol->immutable-string kind) " call"
                                              statically-str "\n"
                                              "  keyword: ~"
                                              (keyword->immutable-string (hash-iterate-key needed (hash-iterate-first needed))))
                               (error-stx))))]
      [(syntax-e (car kws))
       (define kw (syntax-e (car kws)))
       (define needed (hash-remove (or needed-kws orig-needed) kw))
       (define allowed (or allowed-kws
                           (if (pair? a)
                               (and (caddr a)
                                    (for/hasheq ([kw (in-list (caddr a))])
                                      (values kw #t)))
                               #hasheq())))
       (when (and allowed
                  (not (hash-ref allowed kw #f)))
         (raise-syntax-error #f
                             (string-append "keyword argument not recognized by called " (symbol->immutable-string kind)
                                            statically-str "\n"
                                            "  keyword: ~"
                                            (keyword->immutable-string kw))
                             (error-stx)))
       (loop (cdr kws) n needed allowed)]
      [else
       (loop (cdr kws) (add1 n) needed-kws allowed-kws)])))
