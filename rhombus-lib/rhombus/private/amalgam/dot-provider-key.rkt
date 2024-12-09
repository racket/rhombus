#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt"
         "dot-space.rkt")

(provide (for-syntax extract-dot-provider-ids))

;; A `#%dot-provider` value is either
;;  * identifier      ; equivalent to (list identifier)
;;  * (alts alts ...) ; search just first `alts`; rest are "supertypes" for finding common on intersection
;;
;; A alts is
;;  * identifer             ; equivakent to (list identifier)
;;  * (list identifier ...) ; created by union; try each `identifier` until success
;;
;; It's possible for the search through on alts list to be ambigious, especially
;; if a non-checking `:~` is used with a `&&` annotation. Absent a good idea on
;; how to check that, the strategy here is to just try left-to-right.

(define-static-info-key-syntax/provide #%dot-provider
  (static-info-key (lambda (a b)
                     (merge-lists a b))
                   (lambda (a b)
                     (common-tail a b))))

(define-for-syntax (merge-lists a b)
  (let ([as (if (identifier? a) (list a) (syntax->list a))]
        [bs (if (identifier? b) (list b) (syntax->list b))])
    (cond
      [(not (pair? as)) b] ; defensive check: use other if one is ill-formed or empty
      [(not (pair? bs)) a] ; ditto
      [else
       ;; both lists are non-empty, so we can pad by duplicating the first
       (define a-len (length as))
       (define b-len (length bs))
       (map union-alts-ids
            (pad as (- b-len a-len))
            (pad bs (- a-len b-len)))])))

(define-for-syntax (union-alts-ids a b)
  (let ([as (if (identifier? a) (list a) (syntax->list a))]
        [bs (if (identifier? b) (list b) (syntax->list b))])
    (cond
      [(not as) b]
      [(not bs) a]
      [else
       (append
        (for/list ([a (in-list as)]
                   #:unless (for/or ([b (in-list bs)])
                              (free-identifier=? (in-dot-provider-space a)
                                                 (in-dot-provider-space b))))
          a)
        bs)])))

(define-for-syntax (common-tail a b)
  (let ([as (if (identifier? a) (list a) (syntax->list a))]
        [bs (if (identifier? b) (list b) (syntax->list b))])
    (and as ; defensive check
         bs ; ditto
         (let ([a-len (length as)]
               [b-len (length bs)])
           (for/fold ([common '()]
                      #:result (and (pair? common)
                                    (if (and (null? (cdr common))
                                             (identifier? (car common)))
                                        (car common)
                                        common)))
                     ([a (in-list (reverse (list-tail as (max 0 (- a-len b-len)))))]
                      [b (in-list (reverse (list-tail bs (max 0 (- b-len a-len)))))])
             (define sames (common-alts-ids a b))
             #:break (null? sames)
             (if (null? (cdr sames))
                 (cons (car sames) common)
                 (cons sames common)))))))

(define-for-syntax (common-alts-ids a b)
  (let ([as (if (identifier? a) (list a) (syntax->list a))]
        [bs (if (identifier? b) (list b) (syntax->list b))])
    (cond
      [(not as) null]
      [(not bs) null]
      [else
       (for/list ([a (in-list as)]
                  #:when (for/or ([b (in-list bs)])
                           (free-identifier=? (in-dot-provider-space a)
                                              (in-dot-provider-space b))))
         a)])))

(define-for-syntax (pad l n)
  (if (n . > . 0)
      (append
       (for/list ([i (in-range n)]) (car l))
       l)
      l))

(define-for-syntax (extract-dot-provider-ids dp-id/s)
  (cond
    [(not dp-id/s) null]
    [(identifier? dp-id/s) (list dp-id/s)]
    [(pair? (syntax-e dp-id/s))
     (let ([id/s (car (syntax-e dp-id/s))])
       (if (identifier? id/s)
           (list id/s)
           (or (syntax->list id/s) null)))]
    [else null]))
