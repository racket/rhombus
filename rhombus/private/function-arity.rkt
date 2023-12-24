#lang racket/base
(require (for-syntax racket/base
                     racket/keyword
                     racket/symbol
                     shrubbery/property
                     "statically-str.rkt"
                     "hash-set.rkt"))

(provide (for-syntax summarize-arity
                     union-arity-summaries
                     check-arity))

(define-for-syntax (summarize-arity kws defaults rest? kw-rest?)
  (let loop ([kws (if (syntax? kws) (syntax->list kws) kws)]
             [defaults (if (syntax? defaults) (syntax->list defaults) defaults)]
             [bit 1]
             [mask 0]
             [allowed-kws #hasheq()]
             [required-kws #hasheq()])
    (cond
      [(null? kws)
       (define a (bitwise-ior mask
                              (if rest?
                                  (bitwise-xor -1 (sub1 bit))
                                  bit)))
       (if kw-rest?
           `(,a ,(sort (hash-keys required-kws) keyword<?) #f)
           (if (zero? (hash-count allowed-kws))
               a
               `(,a ,(sort (hash-keys required-kws) keyword<?)
                    ,(sort (hash-keys allowed-kws) keyword<?))))]
      [(if (syntax? (car kws)) (syntax-e (car kws)) (car kws))
       (define kw (if (syntax? (car kws)) (syntax-e (car kws)) (car kws)))
       (loop (cdr kws)
             (cdr defaults)
             bit
             mask
             (hash-set allowed-kws kw #t)
             (if (syntax-e (car defaults))
                 required-kws
                 (hash-set required-kws kw #t)))]
      [else
       (loop (cdr kws)
             (cdr defaults)
             (arithmetic-shift bit 1)
             (if (syntax-e (car defaults))
                 (bitwise-ior mask bit)
                 mask)
             allowed-kws
             required-kws)])))

(define-for-syntax (union-arity-summaries as)
  (cond
    [(null? as) #f]
    [(= 1 (length as)) (car as)]
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
