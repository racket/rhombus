#lang racket/base
(require shrubbery/property)

(provide normalize-syntax)

;; Normalize a syntax object that might be from the Racket layer,
;; as in an exception, to make it printable as a shrubbery form
(define (normalize-syntax s)
  (cond
    [(and (syntax? s) (syntax-opaque-raw-property s)) s]
    [else
     (syntax-case* s (multi group
                            parens brackets braces quotes
                            block alts
                            op) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
       [(multi . _) s]
       [(group . _) s]
       [(parens . _) s]
       [(brackets . _) s]
       [(braces . _) s]
       [(quotes . _) s]
       [(block . _) s]
       [(alts . _) s]
       [(op _) s]
       [(_ ...) #`(group . #,s)]
       [_ s])]))
