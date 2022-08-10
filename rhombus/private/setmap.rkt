#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "setmap-parse.rkt"
         "map.rkt"
         (submod "map.rkt" for-info)
         "set.rkt"
         (submod "set.rkt" for-info)
         "static-info.rkt"
         "parse.rkt")

(provide (for-syntax parse-setmap-expression))

(define-for-syntax (parse-setmap-expression stx)
  (define-values (shape args) (parse-setmap-content stx))
  (wrap-static-info*
   (quasisyntax/loc stx
     (#,(if (eq? shape 'set) #'Set-build #'Map-build)
      #,@args))
   (if (eq? shape 'set)
       set-static-info
       map-static-info)))
