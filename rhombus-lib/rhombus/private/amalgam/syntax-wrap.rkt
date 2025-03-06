#lang racket/base

(provide syntax-wrap
         syntax-wrap?
         syntax-wrap-key
         syntax-wrap-attribs
         syntax-unwrap
         syntax*?
         maybe-syntax-wrap)

;; Two possible implementations of wrapped syntax to hold syntax-class
;; fields:
;;
;;   * as a struct
;;
;;   * as a gensym-keyed property
;;
;; A wrapped syntax object is meant to be unwrapped auotamically
;; before it is put into any larger syntax object or before any syntax
;; object on the object, because we don't want extra data for fields
;; kept indefinitely. So, a struct wrapper is ok, and it's safer
;; overall (i.e., detects missing unwraps more often). Using a struct
;; sets up a cross-phase problem for error reporting, however. Using a
;; property avoids trat problem and smooths over potentialy
;; interoperability problems at the small risk of keeping fields alive
;; longer than intended.

#;
(begin
  (struct syntax-wrap (stx key attribs)
    #:transparent
    #:guard (lambda (s k a name)
              (unless (syntax? s)
                (error "bad syntax wrap"))
              (values s k a)))

  (define (syntax-unwrap s)
    (if (syntax-wrap? s)
        (syntax-wrap-stx s)
        s))

  (define (syntax*? v)
    (or (syntax? v)
        (syntax-wrap? v))))

(begin
  (define wrapper-key (gensym 'syntax-wrapper))

  (define (syntax-wrap stx key attribs)
    (syntax-property stx wrapper-key (cons key  attribs)))

  (define (syntax-wrap? v)
    (and (syntax? v)
         (syntax-property v wrapper-key)
         #t))

  (define (syntax-unwrap s)
    (if (syntax? s)
        (syntax-property s wrapper-key #f)
        s))

  (define (syntax-wrap-key s)
    (car (syntax-property s wrapper-key)))

  (define (syntax-wrap-attribs s)
    (cdr (syntax-property s wrapper-key)))

  (define (syntax*? v)
    (syntax? v)))

(define (maybe-syntax-wrap stx depth key attribs)
  (cond
    [(depth . > . 0)
     (define (rotate-hash stx ht)
       ;; convert a hash of lists to a list of hashes
       (for/fold ([hts (for/list ([e (in-list stx)]) #hasheq())]) ([(k vs+d) (in-hash ht)])
         (define d (cdr vs+d))
         (for/list ([ht (in-list hts)]
                    [v (in-list (car vs+d))])
           (hash-set ht k (cons v d)))))
     (for/list ([e (in-list stx)]
                [a (in-list (rotate-hash stx attribs))])
       (maybe-syntax-wrap e (sub1 depth) key a))]
    [(syntax*? stx) (syntax-wrap (syntax-unwrap stx) key attribs)]
    [else stx]))
