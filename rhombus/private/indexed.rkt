#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt")
         "parse.rkt"
         "indexed-ref-set-key.rkt"
         "ref-result-key.rkt"
         "static-info.rkt"
         (only-in "assign.rkt"
                  [= rhombus=]))

(module+ for-ref
  (provide (for-syntax parse-indexed-ref-or-set)))

(define-for-syntax (parse-indexed-ref-or-set indexed stxes)
  (syntax-parse stxes
    #:datum-literals (brackets op)
    #:literals (rhombus=)
    [(_ ((~and head brackets) index) (op rhombus=) . rhs+tail)
     #:with rhs::infix-op+expression+tail #'(rhombus= . rhs+tail)
     (define indexed-set!-id (or (syntax-local-static-info indexed #'#%indexed-set!)
                                 #'indexed-set!))
     (define e (datum->syntax (quote-syntax here)
                              (list indexed-set!-id indexed (index->expression #'index) #'rhs.parsed)
                              (span-srcloc indexed #'head)
                              #'head))
     (values e
             #'rhs.tail)]
    [(_ ((~and head brackets) index) . tail)
     (define indexed-ref-id (or (syntax-local-static-info indexed #'#%indexed-ref)
                                #'indexed-ref))
     (define e (datum->syntax (quote-syntax here)
                              (list indexed-ref-id indexed (index->expression #'index))
                              (span-srcloc indexed #'head)
                              #'head))
     (define result-static-infos (or (syntax-local-static-info indexed #'#%ref-result)
                                     #'()))
     (values (wrap-static-info* e result-static-infos)
             #'tail)]))

(define-for-syntax (index->expression index)
  (syntax-parse index
    #:datum-literals (group)
    [(group key:keyword) #'(quote key)]
    [g #'(rhombus-expression g)]))

(define (indexed-ref indexed index)
  (cond
    [(vector? indexed) (vector-ref indexed index)]
    [(list? indexed) (list-ref indexed index)]
    [(hash? indexed) (hash-ref indexed index)]
    [else
     (raise-argument-error 'indexed-ref "indexed?" indexed)]))

(define (indexed-set! indexed index val)
  (cond
    [(vector? indexed) (vector-set! indexed index val)]
    [(and (hash? indexed) (not (immutable? indexed))) (hash-set! indexed index)]
    [else
     (raise-argument-error 'indexed-set! "mutable-indexed?" indexed)]))
