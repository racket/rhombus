#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "class-primitive.rkt"
         "dot-parse.rkt"
         "realm.rkt"
         "function-arity-key.rkt"
         "define-arity.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Path))

(module+ for-builtin
  (provide path-method-table))

(module+ for-static-info
  (provide (for-syntax path-static-infos)))

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

(define/arity #:name Path.bytes (path-bytes s)
  (bytes->immutable-bytes (path->bytes s)))

(define/arity #:name Path.string (path-string s)
  (string->immutable-string (path->string s)))

(define path-bytes/method (method1 path-bytes))
(define path-string/method (method1 path-string))

(define-primitive-class Path path
  #:constructor-static-info ()
  #:existing
  #:translucent
  #:fields
  ([bytes ()])
  #:properties
  ()
  #:methods
  ([bytes 0 path-bytes path-bytes/method]
   [string 0 path-string path-string/method]))
