#lang racket/base

(provide check-consistent-name)

(define (check-consistent-name stx ids
                               #:has-main? [has-main? #f])
  (define the-id (car ids))
  (for ([another-id (in-list (cdr ids))])
    (unless (bound-identifier=? the-id another-id)
      (raise-syntax-error #f
                          (format "inconsistent name;\n current case name does not match ~a name"
                                  (if has-main? "declared" "initial case"))
                          stx
                          another-id))))
