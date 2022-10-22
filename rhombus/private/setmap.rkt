#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "setmap-parse.rkt"
         "map.rkt"
         (submod "map.rkt" append)
         (submod "map.rkt" for-info)
         "set.rkt"
         (submod "set.rkt" append)
         (submod "set.rkt" for-info)
         "static-info.rkt"
         "parse.rkt")

(provide (for-syntax parse-setmap-expression))

(define-for-syntax (parse-setmap-expression stx)
  (define-values (shape args maybe-rest) (parse-setmap-content stx))
  (define without-rest
    (wrap-static-info*
     (quasisyntax/loc stx
       (#,(if (eq? shape 'set) #'Set-build #'Map-build)
        #,@args))
     (if (eq? shape 'set)
         set-static-info
         map-static-info)))
  (if maybe-rest
      (wrap-static-info*
       (quasisyntax/loc stx
         (#,(if (eq? shape 'set) #'set-append #'hash-append)
          #,without-rest
          #,maybe-rest))
       (if (eq? shape 'set)
           set-static-info
           map-static-info))
      without-rest))
