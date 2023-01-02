#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "setmap-parse.rkt"
         "map.rkt"
         (submod "map.rkt" for-build)
         (submod "map.rkt" for-info)
         (submod "map.rkt" for-binding)
         "set.rkt"
         (submod "set.rkt" for-build)
         (submod "set.rkt" for-info)
         (submod "set.rkt" for-binding)
         "static-info.rkt"
         "parse.rkt")

(provide (for-syntax parse-setmap-expression
                     parse-setmap-binding))

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
  
(define-for-syntax (parse-setmap-binding who stx)
  ;; we use `parse-setmap-content` just to pick between maps and sets;
  ;; we'll then parse from scarcth, since bindings support a more
  ;; limited set of splices
  (define-values (shape argss)
    (syntax-parse stx
      [(_ braces . tail)
       (parse-setmap-content #'braces
                             #:who who
                             #:raw? #t)]))
  (if (eq? shape 'set)
      (parse-set-binding who stx "braces")
      (parse-map-binding who stx "braces")))
