#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "pack.rkt"
                     "srcloc.rkt")
         "parse.rkt")

(provide (for-syntax wrap-expression))

(define-for-syntax (wrap-expression form #:srcloc [loc (maybe-respan form)])
  (relocate
   loc
   (syntax-parse form
     #:datum-literals (parsed group multi)
     [(multi (group (parsed #:rhombus/expr e))) #'e] ; shortcut
     [(group (parsed #:rhombus/expr e)) #'e]         ; shortcut
     [(parsed #:rhombus/expr e) #'e]                 ; shortcut
     [_ #`(rhombus-expression #,(unpack-group form 'expression #f))])))
