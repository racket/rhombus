#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "class-primitive.rkt"
         "annotation-failure.rkt"
         "call-result-key.rkt"
         "define-arity.rkt"
         "compare-key.rkt"
         (submod "annotation.rkt" for-class)
         (submod "string.rkt" static-infos)
         (submod "bytes.rkt" static-infos))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Path)
         (for-space rhombus/annot
                    PathString))

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
    [else (raise-annotation-failure who c "String || Bytes || Path")]))

(define/method (Path.bytes s)
  #:primitive (path->bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (bytes->immutable-bytes (path->bytes s)))

(define/method (Path.string s)
  #:primitive (path->string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (path->string s)))

(define-annotation-syntax PathString (identifier-annotation path-string? ()))
