#lang racket/base
(require (for-syntax racket/base
                     racket/keyword
                     racket/symbol
                     shrubbery/property
                     "statically-str.rkt"))

(provide (for-syntax summarize-arity
                     shift-arity
                     union-arity-summaries
                     intersect-arity-summaries
                     check-arity))

;; A function arity description is one of
;;  <mask>
;;  (<mask> (<required-kw> ...) (<allowed-kw> ...))
;;  (<mask> (<required-kw> ...) #f) ; = all allowed

(define-for-syntax (summarize-arity kws defaults rest? kw-rest?)
  (define (syntax->list/maybe stx)
    (if (syntax? stx) (syntax->list stx) stx))
  (define (make-arity bit mask allowed-kws required-kws)
    (define a (bitwise-ior mask
                           (if rest?
                               (bitwise-xor -1 (sub1 bit))
                               bit)))
    (cond
      [kw-rest? (list a (sort required-kws keyword<?) #f)]
      [(null? allowed-kws) a]
      [else (list a (sort required-kws keyword<?) (sort allowed-kws keyword<?))]))
  (for/fold ([bit 1]
             [mask 0]
             [allowed-kws '()]
             [required-kws '()]
             #:result (make-arity bit mask allowed-kws required-kws))
            ([kw (in-list (syntax->list/maybe kws))]
             [default (in-list (syntax->list/maybe defaults))])
    (cond
      [(syntax-e kw)
       (values bit
               mask
               (cons (syntax-e kw) allowed-kws)
               (if (syntax-e default)
                   required-kws
                   (cons (syntax-e kw) required-kws)))]
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

(define-for-syntax (list->hash l)
  (for/hasheq ([v (in-list l)])
    (values v #t)))

(define-for-syntax (hash->list ht)
  (sort (hash-keys ht) keyword<?))

(define-for-syntax (hash-intersect a b)
  (let-values ([(a b)
                (if ((hash-count a) . < . (hash-count b))
                    (values b a)
                    (values a b))])
    (for/hasheq ([k (in-immutable-hash-keys b)]
                 #:when (hash-ref a k #f))
      (values k #t))))

(define-for-syntax (hash-union a b)
  (let-values ([(a b)
                (if ((hash-count a) . < . (hash-count b))
                    (values b a)
                    (values a b))])
    (for/fold ([a a]) ([k (in-immutable-hash-keys b)])
      (hash-set a k #t))))

(define-for-syntax (combine-arity-summaries as combine)
  (cond
    [(null? as) 0]
    [(null? (cdr as)) (car as)]
    [(andmap values as)
     (define (normalize a)
       (if (pair? a)
           (list (car a) (list->hash (cadr a)) (and (caddr a) (list->hash (caddr a))))
           (list a #hasheq() #hasheq())))
     (define norm-a
       (for/fold ([new-a (normalize (car as))]) ([a (in-list (cdr as))])
         (combine new-a (normalize a))))
     (define required-kws (hash->list (cadr norm-a)))
     (define allowed-kws (and (caddr norm-a) (hash->list (caddr norm-a))))
     (if (and (null? required-kws)
              (null? allowed-kws))
         (car norm-a)
         (list (car norm-a) required-kws allowed-kws))]
    [else #f]))

(define-for-syntax (union-arity-summaries as)
  (combine-arity-summaries
   as
   (lambda (new-a a)
     (list (bitwise-ior (car new-a) (car a))
           (hash-intersect (cadr new-a) (cadr a))
           (and (caddr new-a) (caddr a) (hash-union (caddr new-a) (caddr a)))))))

(define-for-syntax (intersect-arity-summaries as)
  (combine-arity-summaries
   as
   (lambda (new-a a)
     (list (bitwise-and (car new-a) (car a))
           (hash-union (cadr new-a) (cadr a))
           (and (caddr new-a) (caddr a) (hash-intersect (caddr new-a) (caddr a)))))))

;; `kind` = #f => return boolean, `stx` and `fallback-stx` unused;
;; if `always?` is true, then report not just whether the arity might might,
;; but whether it definitely matches
(define-for-syntax (check-arity stx fallback-stx a n kws rsts kwrsts kind #:always? [always? #f])
  (define orig-needed (if (pair? a)
                          (list->hash (cadr a))
                          #hasheq()))
  (define (error-stx) (or stx (let ([s (or (syntax-opaque-raw-property fallback-stx)
                                           (syntax-raw-property fallback-stx))])
                                (if (string? s)
                                    (datum->syntax #f (string->symbol s) fallback-stx)
                                    fallback-stx))))
  (let loop ([kws kws] [n n] [needed-kws #f] [allowed-kws #f])
    (cond
      [(null? kws)
       (and
        (if (and a
                 (zero? (bitwise-and (if rsts
                                         (bitwise-not (sub1 (arithmetic-shift 1 n)))
                                         (arithmetic-shift 1 n))
                                     (if (pair? a) (car a) a))))
            (and kind
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
            #t)
        (if kwrsts
            #t
            (let ()
              (define needed (or needed-kws orig-needed))
              (if (and needed ((hash-count needed) . > . 0))
                  (and kind
                       (raise-syntax-error
                        #f
                        (string-append "missing keyword argument in " (symbol->immutable-string kind) " call"
                                       statically-str "\n"
                                       "  keyword: ~"
                                       (keyword->immutable-string (hash-iterate-key needed (hash-iterate-first needed))))
                        (error-stx)))
                  #t)))
        (or (not always?)
            (and
             a
             (or (not rsts)
                 (let ([all (bitwise-not (sub1 (arithmetic-shift 1 n)))])
                   (= all (bitwise-and all (if (pair? a) (car a) a)))))
             (or (not kwrsts)
                 (and (pair? a)
                      (not (caddr a))
                      (let ()
                        (= 0 (hash-count (or needed-kws orig-needed)))))))))]
      [(syntax-e (car kws))
       (define kw (syntax-e (car kws)))
       (define needed (hash-remove (or needed-kws orig-needed) kw))
       (define allowed (or allowed-kws
                           (if (pair? a)
                               (and (caddr a)
                                    (list->hash (caddr a)))
                               #hasheq())))
       (and (if (and allowed
                     (not (hash-ref allowed kw #f)))
                (and kind
                     (raise-syntax-error
                      #f
                      (string-append "keyword argument not recognized by called " (symbol->immutable-string kind)
                                     statically-str "\n"
                                     "  keyword: ~"
                                     (keyword->immutable-string kw))
                      (error-stx)))
                #t)
            (loop (cdr kws) n needed allowed))]
      [else
       (loop (cdr kws) (add1 n) needed-kws allowed-kws)])))
