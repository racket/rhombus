#lang racket/base
(require racket/keyword)

(provide check-consistent)

(define (check-consistent #:who [who #f] #:has-main? [has-main? #f] stx ids what)
  (define the-id (car ids))
  (for ([another-id (in-list (cdr ids))])
    (unless (free-identifier=? the-id another-id)
      (raise-syntax-error (if (keyword? who)
                              (string->symbol (keyword->immutable-string who))
                              who)
                          (format "case ~a does not match the ~a ~a"
                                  what
                                  (if has-main? "declared" "initial case")
                                  what)
                          stx
                          another-id))))
