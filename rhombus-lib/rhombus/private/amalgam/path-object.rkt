#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "class-primitive.rkt"
         "annotation-failure.rkt"
         "call-result-key.rkt"
         "define-arity.rkt"
         "append-key.rkt"
         "compare-key.rkt"
         "index-result-key.rkt"
         "static-info.rkt"
         (submod "annotation.rkt" for-class)
         (submod "bytes.rkt" static-infos)
         (submod "function.rkt" for-info)
         (submod "list.rkt" for-listable)
         (submod "string.rkt" static-infos))

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
  #:instance-static-info ((#%append Path.extend)
                          (#%compare ((< path<?)
                                      (<= path<=?)
                                      (> path>?)
                                      (>= path>=?)
                                      (= equal?)
                                      (!= path!=?))))
  #:existing
  #:translucent
  #:fields
  ([bytes Path.bytes #,(get-bytes-static-infos)])
  #:namespace-fields
  ([current_directory current-directory])
  #:properties
  ()
  #:methods
  (bytes
   extend
   is_absolute
   parts
   string))

(define/arity #:name Path (path c)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (cond
    [(path? c) c]
    [(bytes? c) (bytes->path c)]
    [(string? c) (string->path c)]
    [else (raise-annotation-failure who c "String || Bytes || Path")]))

(define-static-info-syntax current-directory
  (#%function-arity 3)
  (#%call-result #,(get-path-static-infos))
  . #,(get-function-static-infos))

(define/method (Path.bytes s)
  #:primitive (path->bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (bytes->immutable-bytes (path->bytes s)))

(define/method (Path.extend p . ss)
  #:primitive (build-path)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (apply build-path p ss))

(define/method (Path.is_absolute p)
  #:primitive (absolute-path?)
  (absolute-path? p))

(define/method (Path.parts p)
  #:primitive (explode-path)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (to-treelist #f (explode-path p)))

(define/method (Path.string s)
  #:primitive (path->string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (path->string s)))

(define-annotation-syntax PathString (identifier-annotation path-string? ()))
