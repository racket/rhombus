#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "setmap-parse.rkt"
         "map.rkt"
         (submod "map.rkt" for-build)
         (submod "map.rkt" for-info)
         "set.rkt"
         (submod "set.rkt" for-build)
         (submod "set.rkt" for-info)
         "static-info.rkt"
         "parse.rkt")

(provide (for-syntax parse-setmap-expression))

(define-for-syntax (parse-setmap-expression stx
                                             #:shape [init-shape #f]
                                             #:who [who #f]
                                             #:repetition? [repetition? #f])
  (define-values (shape argss)
    (parse-setmap-content stx
                          #:shape init-shape
                          #:who who
                          #:repetition? repetition?
                          #:list->set #'list->set
                          #:list->map #'list->map))
  (build-setmap stx
                argss
                (if (eq? shape 'set) #'Set-build #'Map-build)
                (if (eq? shape 'set) #'set-extend* #'hash-extend*)
                (if (eq? shape 'set) #'set-append #'hash-append)
                (if (eq? shape 'set) #'set-assert #'hash-assert)
                (if (eq? shape 'set) set-static-info map-static-info)
                #:repetition? repetition?
                #:list->setmap (if (eq? shape 'set) #'list->set #'list->map)))
  
