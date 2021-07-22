#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/provide-transform
                     "format-id.rkt")
         racket/provide-syntax)

(provide property-out)

(define-provide-syntax (property-out stx)
  (syntax-parse stx
    [(_ name:identifier)
     (with-syntax ([prop:name (format-id "prop:~a" #'name)]
                   [name-ref (format-id "~a-ref" #'name)]
                   [name? (format-id "~a?" #'name)])
       #'(combine-out prop:name
                      name
                      name?
                      name-ref))]))
