#lang racket/base
(require (for-syntax racket/base
                     version/utils))

(provide meta-if-version-at-least)

(define-syntax (meta-if-version-at-least stx)
  (syntax-case stx ()
    [(_ vers a b)
     (if (version<? (version) (syntax-e #'vers))
         #'b
         #'a)]))
