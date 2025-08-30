#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "static-info.rkt")

(provide (for-syntax extract-index-uniform-result
                     extract-index-result
                     or-index-results
                     add-index-result))

;; An #%index-result value is either
;;   (#:at_index default-static-infos (key static-infos) ...)
;;   static-infos  => equivalent to (#:at_index static-infos)
;; where `default-static-infos` can be #f to indicate that the `key`s are
;; the only possibilities, and `default-static-infos` needs
;; to be "or"ed with all `static-infos` for an unknown key

(define-static-info-key-syntax/provide #%index-result
  (static-info-key (lambda (a b)
                     (or-index-results a b))
                   (lambda (a b)
                     (and-index-results a b))))

(define-for-syntax (extract-index-uniform-result si)
  (cond
    [(not si) #f]
    [else
     (syntax-parse si
       [(#:at_index #f)
        #'()]
       [(#:at_index #f (idx0 i-si0) (idx i-si) ...)
        (for/fold ([si #'i-si0]) ([i-si (syntax->list #'(i-si ...))])
          (static-infos-or si i-si))]
       [(#:at_index other-si (idx i-si) ...)
        (for/fold ([si #'other-si]) ([i-si (syntax->list #'(i-si ...))])
          (static-infos-or si i-si))]
       [else si])]))

(define-for-syntax (extract-index-result si key)
  (cond
    [(not si) #f]
    [else
     (syntax-parse si
       [(#:at_index other-si (idx i-si) ...)
        (or (for/or ([idx (in-list (syntax->list #'(idx ...)))]
                     [i-si (in-list (syntax->list #'(i-si ...)))])
              (and (equal? key (syntax-e idx))
                   i-si))
            (if (syntax-e #'other-si)
                #'other-si
                #'()))]
       [else si])]))

(define-for-syntax (parse-index-results si)
  (syntax-parse si
    [(#:at_index other (idx si) ...)
     (values (and (syntax-e #'other)
                  #'other)
             (for/hash ([idx (in-list (syntax->list #'(idx ...)))]
                        [si (in-list (syntax->list #'(si ...)))])
               (values (syntax-e idx) si)))]
    [_ (values si #hash())]))

(define-for-syntax (add-index-result si i i-isi)
  (syntax-parse si
    [(#:at_index other-si i-case ...)
     #`(#:at_index other-si (#,i #,i-isi) i-case ...)]
    [else
     #`(#:at_index #,si (#,i #,i-isi))]))

(define-for-syntax (or-index-results a b)
  (define-values (a-default a-ht) (parse-index-results a))
  (define-values (b-default b-ht) (parse-index-results b))
  (define defaults (and (or a-default b-default)
                        (static-infos-or (or a-default #'()) (or b-default #'()))))
  (cond
    [(and (= 0 (hash-count a-ht))
          (= 0 (hash-count b-ht)))
     defaults]
    [else
     (define-values (new-defaults new-ht)
       (for/fold ([new-defaults defaults] [new-ht #hash()]) ([(key a-si) (in-hash a-ht)])
         (cond
           [(hash-ref b-ht key #f)
            => (lambda (b-si)
                 (values new-defaults (hash-set new-ht key (static-infos-or a-si b-si))))]
           [else
            (values (if new-defaults (static-infos-or new-defaults a-si) a-si) new-ht)])))
     (define all-defaults
       (for/fold ([new-defaults new-defaults]) ([(key b-si) (in-hash b-ht)])
         (cond
           [(hash-ref a-ht key #f) new-defaults]
           [else (if new-defaults (static-infos-or new-defaults b-si) b-si)])))
     (if (= 0 (hash-count new-ht))
         all-defaults
         #`(#:at_index #,all-defaults
            #,@(for/list ([(key si) (in-hash new-ht)])
                 #`(#,key #,si))))]))

(define-for-syntax (and-index-results a b)
  (define-values (a-default a-ht) (parse-index-results a))
  (define-values (b-default b-ht) (parse-index-results b))
  (define defaults (and (or a-default b-default)
                        (static-infos-and (or a-default #'()) (or b-default #'()))))
  (cond
    [(and (= 0 (hash-count a-ht))
          (= 0 (hash-count b-ht)))
     defaults]
    [else
     (define new-ht
       (for/fold ([new-ht #hash()]) ([(key a-si) (in-hash a-ht)])
         (cond
           [(hash-ref b-ht key #f)
            => (lambda (b-si)
                 (hash-set new-ht key (static-infos-and a-si b-si)))]
           [else
            (hash-set new-ht key (if b-default (static-infos-and b-default a-si) a-si))])))
     (define all-ht
       (for/fold ([new-ht new-ht]) ([(key b-si) (in-hash b-ht)])
         (cond
           [(hash-ref a-ht key #f) new-ht]
           [else
            (hash-set new-ht key (if a-default (static-infos-and a-default b-si) b-si))])))
     (if (= 0 (hash-count all-ht))
         defaults
         #`(#:at_index #,defaults
            #,@(for/list ([(key si) (in-hash all-ht)])
                 #`(#,key #,si))))]))
