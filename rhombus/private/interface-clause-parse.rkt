#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     (only-in "class-parse.rkt"
                              added-method))
         "interface-clause.rkt"
         "class-clause-parse.rkt"
         (submod "class-clause-parse.rkt" for-interface)
         "parens.rkt")

(provide (for-syntax parse-options))

;; interface clause forms are defined in "class-clause-parse.rkt"

(define-for-syntax (parse-options orig-stx forms)
  (syntax-parse forms
    #:context orig-stx
    [((_ clause-parsed) ...)
     (define clauses (syntax->list #'(clause-parsed ...)))
     (define (extract-rhs b)
       (syntax-parse b
         [(_::block g) #'g]
         [else
          (raise-syntax-error #f
                              "expected a single entry point in block body"
                              b)]))
     (let loop ([clauses clauses] [options #hasheq()])
       (cond
         [(null? clauses) options]
         [else
          (define clause (car clauses))
          (define new-options
            (syntax-parse clause
              #:literals (extends method private override abstract final final-override internal)
              [(internal id)
               (when (hash-has-key? options 'internal)
                 (raise-syntax-error #f "multiple internal-name clauses" orig-stx clause))
               (hash-set options 'internal #'id)]
              [(extends id ...)
               (hash-set options 'extends (append (reverse (syntax->list #'(id ...)))
                                                  (hash-ref options 'extends '())))]
              [((~and tag (~or method override private final final-override)) id rhs)
               (hash-set options 'methods (cons (added-method #'id
                                                              (car (generate-temporaries #'(id)))
                                                              #'rhs
                                                              (syntax-e #'tag))
                                                (hash-ref options 'methods null)))]
              [(abstract id)
               (hash-set options 'methods (cons (added-method #'id
                                                              '#:abstract
                                                              #f
                                                              'abstract)
                                                (hash-ref options 'methods null)))]
              [_
               (raise-syntax-error #f "unrecognized clause" orig-stx clause)]))
          (loop (cdr clauses) new-options)]))]))
