#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt")
         "expression.rkt"
         "parse.rkt"
         "map-ref-set-key.rkt"
         "ref-result-key.rkt"
         "static-info.rkt"
         (only-in "assign.rkt"
                  [= rhombus=])
         (submod "set.rkt" for-ref))

(provide ++)

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
                              (list map-set!-id map #'(rhombus-expression index) #'rhs.parsed)
                              (span-srcloc map #'head)
                              #'head))
     (values e
             #'rhs.tail)]
    [(_ ((~and head brackets) index) . tail)
     (define map-ref-id (or (syntax-local-static-info map #'#%map-ref)
                                #'map-ref))
     (define e (datum->syntax (quote-syntax here)
                              (list map-ref-id map #'(rhombus-expression index))
                              (span-srcloc map #'head)
                              #'head))
     (define result-static-infos (or (syntax-local-static-info map #'#%ref-result)
                                     #'()))
     (values (wrap-static-info* e result-static-infos)
             #'tail)]))

(define (map-ref map index)
  (cond
    [(vector? map) (vector-ref map index)]
    [(list? map) (list-ref map index)]
    [(hash? map) (hash-ref map index)]
    [(set? map) (hash-ref (set-ht map) index #f)]
    [else
     (raise-argument-error 'map-ref "map?" map)]))

(define (map-set! map index val)
  (cond
    [(vector? map) (vector-set! map index val)]
    [(and (hash? map) (not (immutable? map))) (hash-set! map index)]
    [(and (set? map) (not (immutable? (set-ht map)))) (if val
                                                          (hash-set! (set-ht map) index #t)
                                                          (hash-remove! (set-ht map) index))]
    [else
     (raise-argument-error 'map-set! "mutable-map?" map)]))

(define-syntax ++
  (expression-infix-operator
   (quote-syntax ++)
   null
   'automatic
   (lambda (form1 form2 stx)
     (define append-id (or (syntax-local-static-info form1 #'#%map-append)
                          #'map-append))
     (datum->syntax (quote-syntax here)
                    (list append-id form1 form2)
                    (span-srcloc form1 form2)
                    stx))
   'left))

(define (map-append map1 map2)
  (cond
    [(vector? map1) (raise-arguments-error '++
                                           "cannot extend a plain array"
                                           "array" map1
                                           "other value" map2)]
    [(list? map1) (cond
                    [(list? map2) (append map1 map2)]
                    [(vector? map2) (append map1 (vector->list map2))]
                    [else (raise-arguments-error '++
                                                 "cannot append a list and other value"
                                                 "list" map1
                                                 "other value" map2)])]
    [(hash? map1) (cond
                    [(hash? map2) (for/fold ([ht map1]) ([(k v) (in-hash map2)])
                                    (hash-set ht k v))]
                    [else (raise-arguments-error '++
                                                 "cannot append a hash map and other value"
                                                 "hash map" map1
                                                 "other value" map2)])]
    [(set? map1) (cond
                   [(set? map2) (set (for/fold ([ht (set-ht map1)]) ([k (in-hash-keys (set-ht map2))])
                                       (hash-set ht k #t)))]
                   [else (raise-arguments-error '++
                                                "cannot append a set and other value"
                                                "set" map1
                                                "other value" map2)])]
    [else (raise-argument-error '++ "set?" map1)]))
