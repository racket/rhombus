#lang racket/base
(require (submod "annotation.rkt" for-class))

(provide cross-path-order)

(define (cross-path-order who a b)
  (cond
    [(path? a)
     (cond
       [(path? b) (cond
                    [(equal? a b) 0]
                    [(a . path<? . b) -1]
                    [else 1])]
       [else
        (unless (or (not who)
                    (path-for-some-system? b))
          (raise-annotation-failure who b "CrossPath"))
        (if (eq? (path-convention-type a) 'unix)
            -1
            1)])]
    [else
     (unless (or (not who)
                 (and (path-for-some-system? a)
                      (path-for-some-system? b)))
       (raise-annotation-failure who (if (path-for-some-system? a) b a) "CrossPath"))
     (cond
       [(eq? (path-convention-type a)
             (path-convention-type b))
        (define a-bstr (path->bytes a))
        (define b-bstr (path->bytes b))
        (cond
          [(bytes=? a-bstr b-bstr) 0]
          [(a-bstr . bytes<? . b-bstr) -1]
          [else 1])]
       [else
        (if (eq? (path-convention-type a) 'unix)
            -1
            1)])]))
