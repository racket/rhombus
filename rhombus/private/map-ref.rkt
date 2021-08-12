#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt")
         "parse.rkt"
         "map-ref-set-key.rkt"
         "ref-result-key.rkt"
         "static-info.rkt"
         (only-in "assign.rkt"
                  [= rhombus=]))

(module+ for-ref
  (provide (for-syntax parse-map-ref-or-set)))

(define-for-syntax (parse-map-ref-or-set map stxes)
  (syntax-parse stxes
    #:datum-literals (brackets op)
    #:literals (rhombus=)
    [(_ ((~and head brackets) index) (op rhombus=) . rhs+tail)
     #:with rhs::infix-op+expression+tail #'(rhombus= . rhs+tail)
     (define map-set!-id (or (syntax-local-static-info map #'#%map-set!)
                                 #'map-set!))
     (define e (datum->syntax (quote-syntax here)
                              (list map-set!-id map (index->expression #'index) #'rhs.parsed)
                              (span-srcloc map #'head)
                              #'head))
     (values e
             #'rhs.tail)]
    [(_ ((~and head brackets) index) . tail)
     (define map-ref-id (or (syntax-local-static-info map #'#%map-ref)
                                #'map-ref))
     (define e (datum->syntax (quote-syntax here)
                              (list map-ref-id map (index->expression #'index))
                              (span-srcloc map #'head)
                              #'head))
     (define result-static-infos (or (syntax-local-static-info map #'#%ref-result)
                                     #'()))
     (values (wrap-static-info* e result-static-infos)
             #'tail)]))

(define-for-syntax (index->expression index)
  (syntax-parse index
    #:datum-literals (group)
    [(group key:keyword) #'(quote key)]
    [g #'(rhombus-expression g)]))

(define (map-ref map index)
  (cond
    [(vector? map) (vector-ref map index)]
    [(list? map) (list-ref map index)]
    [(hash? map) (hash-ref map index)]
    [else
     (raise-argument-error 'map-ref "map?" map)]))

(define (map-set! map index val)
  (cond
    [(vector? map) (vector-set! map index val)]
    [(and (hash? map) (not (immutable? map))) (hash-set! map index)]
    [else
     (raise-argument-error 'map-set! "mutable-map?" map)]))
