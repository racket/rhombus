#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "pack.rkt")
         "parse.rkt")

(provide (for-syntax wrap-expression))

(define-for-syntax (wrap-expression form)
  (syntax-parse form
    #:datum-literals (parsed group multi)
    [(multi (group (parsed e))) #'e] ; shortcut
    [(group (parsed e)) #'e]         ; shortcut
    [(parsed e) #'e]                 ; shortcut
    [_ #`(rhombus-expression #,(unpack-group form 'expression #f))]))
