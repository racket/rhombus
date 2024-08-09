#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "class-primitive.rkt"
         "realm.rkt"
         "call-result-key.rkt"
         "define-arity.rkt"
         "compare-key.rkt"
         (submod "string.rkt" static-infos)
         (submod "bytes.rkt" static-infos))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Path))

(module+ for-builtin
  (provide path-method-table))

(module+ for-static-info
  (provide (for-syntax get-path-static-infos)))

(define (path<=? p q)
  (or (equal? p q) (path<? p q)))

(define (path!=? p q)
  (not (equal? p q)))

(define (path>=? p q)
  (or (equal? p q) (path<? q p)))

(define (path>? p q)
  (path<? q p))

(define-primitive-class Path path
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%compare ((< path<?)
                                      (<= path<=?)
                                      (> path>?)
                                      (>= path>=?)
                                      (= equal?)
                                      (!= path!=?))))
  #:existing
  #:translucent
  #:fields
  ([bytes Path.bytes #,(get-bytes-static-infos)])
  #:properties
  ()
  #:methods
  (bytes
   string
   ))

(define/arity #:name Path (path c)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (cond
    [(path? c) c]
    [(bytes? c) (bytes->path c)]
    [(string? c) (string->path c)]
    [else (raise-argument-error* who rhombus-realm
                                 "String || Bytes || Path" c)]))

(define/method (Path.bytes s)
  #:inline
  #:primitive (path->bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (bytes->immutable-bytes (path->bytes s)))

(define/method (Path.string s)
  #:inline
  #:primitive (path->string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (path->string s)))
