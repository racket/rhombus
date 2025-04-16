#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "setmap-parse.rkt"
         (submod "map.rkt" for-build)
         (submod "map.rkt" for-info)
         (submod "map.rkt" for-binding)
         (submod "set.rkt" for-build)
         (submod "set.rkt" for-info)
         (submod "set.rkt" for-binding)
         "static-info.rkt"
         "sequence-element-key.rkt"
         "index-result-key.rkt"
         "values-key.rkt")

(provide (for-syntax parse-setmap-expression
                     parse-setmap-binding))

(define-for-syntax (parse-setmap-expression stx
                                             #:shape [init-shape #f]
                                             #:who [who #f]
                                             #:repetition? [repetition? #f])
  (define-values (shape argss k-static-infos v-static-infos)
    (parse-setmap-content stx
                          #:set-for-form #'for/setalw
                          #:shape init-shape
                          #:who who
                          #:repetition? repetition?))
  (relocate-wrapped
   (respan stx)
   (build-setmap stx
                 argss
                 (if (eq? shape 'set) #'Set-build #'Map-build)
                 (if (eq? shape 'set) #'set-extend* #'hash-extend*)
                 (if (eq? shape 'set) #'set-append #'hash-append)
                 (if (eq? shape 'set) #'set-assert #'hash-assert)
                 (if (eq? shape 'set)
                     (if (static-infos-empty? k-static-infos)
                         (get-set-static-infos)
                         #`((#%sequence-element #,k-static-infos)
                            #,@(get-set-static-infos)))
                     (if (and (static-infos-empty? k-static-infos)
                              (static-infos-empty? v-static-infos))
                         (get-map-static-infos)
                         #`((#%index-result #,v-static-infos)
                            (#%sequence-element ((#%values (#,k-static-infos #,v-static-infos))))
                            #,@(get-map-static-infos))))
                 #:repetition? repetition?
                 #:rep-for-form (if (eq? shape 'set) #'for/setalw #'for/hashalw))))

(define-for-syntax (parse-setmap-binding who stx)
  ;; we use `parse-setmap-content` just to pick between maps and sets;
  ;; we'll then parse from scarcth, since bindings support a more
  ;; limited set of splices
  (define-values (shape argss k-static-infos v-static-infos)
    (syntax-parse stx
      [(_ braces . tail)
       (parse-setmap-content #'braces
                             #:who who
                             #:raw? #t)]))
  (if (eq? shape 'set)
      (parse-set-binding who stx "braces")
      (parse-map-binding who stx "braces")))
