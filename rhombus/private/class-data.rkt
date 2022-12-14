#lang racket/base
(require syntax/parse)

(provide (struct-out class-data)
         class-data-internal-info)

(struct class-data (stx))

(define (class-data-internal-info data k)
  (syntax-parse (class-data-stx data)
    [(_ base-stx scope-stx
        full-name name name?
        maybe-internal-name-instance
        _
        constructor-field-keywords
        _ _ _ _
        constructor-name-fields)
     (k (lambda (stx) ((make-syntax-delta-introducer #'scope-stx #'base-stx) stx 'remove))
        #'name? #'maybe-internal-name-instance
        #'constructor-name-fields
        #'constructor-field-keywords)]))
