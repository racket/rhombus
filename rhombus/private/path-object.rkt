#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "name-root.rkt"
         "expression.rkt"
         "expression+binding.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "dot-parse.rkt"
         "call-result-key.rkt"
         "composite.rkt"
         "realm.rkt")

(provide Path
         (for-space rhombus/annot Path))

(module+ for-builtin
  (provide path-method-table))

(module+ for-static-info
  (provide (for-syntax path-static-infos)))

(define-for-syntax path-static-infos
  #'((#%dot-provider path-instance)))

(define-static-info-syntax path
  (#%call-result #,path-static-infos))

(define-annotation-syntax Path
  (identifier-annotation #'Path #'path? path-static-infos))

(define path
  (let ([Path (lambda (c)
                (cond
                  [(path? c) c]
                  [(bytes? c) (bytes->path c)]
                  [(string? c) (string->path c)]
                  [else (raise-argument-error* 'Path
                                               rhombus-realm
                                               "String || Bytes || Path"
                                               c)]))])
    Path))

(define-name-root Path
  #:root (make-expression+binding-prefix-operator
          #'Path
          '((default . stronger))
          'macro
          (lambda (stx)
            (syntax-parse stx
              [(_ . tail)
               (values #'path #'tail)]))
          (make-composite-binding-transformer "Path"
                                              #'path?
                                              (list #'path->bytes)
                                              #'(())))
  #:fields
  ([bytes path->bytes]
   [string path->string]))

(define (Path.bytes s)
  (bytes->immutable-bytes (path->bytes s)))

(define (Path.string s)
  (string->immutable-string (path->string s)))

(define path-method-table
  (hash 'bytes (method1 Path.bytes)
        'string (method1 Path.string)))

(define-syntax path-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(bytes) (0ary #'Path.bytes)]
        [(string) (0ary #'Path.string)]
        [else (fail-k)])))))
