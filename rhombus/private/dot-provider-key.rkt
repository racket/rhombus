#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt"
         "dot-space.rkt")

(provide (for-syntax extract-dot-provider-id))

(define-static-info-key-syntax/provide #%dot-provider
  (static-info-key (lambda (a b)
                     (cond
                       [(and (identifier? a) (identifier? b))
                        (static-info-identifier-union a b)]
                       [else
                        ;; use `b` if it's more information than `a`, otherwise use `a`:
                        (let ([as (if (identifier? a) (list a) (syntax->list a))]
                              [bs (if (identifier? b) (list b) (syntax->list b))])
                          (or (and as
                                   bs
                                   (let ([a-len (length as)]
                                         [b-len (length bs)])
                                     (and (> b-len a-len)
                                          (for/or ([a (in-list as)]
                                                   [b (in-list (list-tail bs (- b-len a-len)))])
                                            (free-identifier=? (in-dot-provider-space a)
                                                               (in-dot-provider-space b)))))
                                   b)
                              a))]))
                   (lambda (a b)
                     (let ([as (if (identifier? a) (list a) (syntax->list a))]
                           [bs (if (identifier? b) (list b) (syntax->list b))])
                       (and as
                            bs
                            (let ([a-len (length as)]
                                  [b-len (length bs)])
                              (let loop ([as (list-tail as (max 0 (- a-len b-len)))]
                                         [bs (list-tail bs (max 0 (- b-len a-len)))])
                                (cond
                                  [(null? as) '()]
                                  [else
                                   (define common (loop (cdr as) (cdr bs)))
                                   (if (free-identifier=? (in-dot-provider-space (car as))
                                                          (in-dot-provider-space (car bs)))
                                       (cons (car as) common)
                                       (and (pair? common)
                                            (if (null? (cdr common))
                                                (car common)
                                                common)))]))))))))

(define-for-syntax (extract-dot-provider-id dp-id/s)
  (cond
    [(not dp-id/s) #false]
    [(identifier? dp-id/s) dp-id/s]
    [(pair? (syntax-e dp-id/s)) (car (syntax-e dp-id/s))]
    [else #f]))
