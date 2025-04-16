#lang racket/base
(require (for-syntax racket/base
                     racket/stxparam-exptime)
         racket/stxparam)

;; see "class-this-id.rkt" for the binding of `this`

(provide this-id
         private-tables

         (for-syntax get-private-tables))

(define-syntax-parameter this-id #f)
(define-syntax-parameter private-tables #f)

(define-for-syntax (get-private-tables)
  (let ([id/table (syntax-parameter-value #'private-tables)])
    (if id/table
        (if (identifier? id/table)
            (syntax-local-value id/table)
            id/table)
        '())))

