#lang racket/base
(require (for-syntax racket/base)
	 racket/path
         "provide.rkt"
         "class-primitive.rkt"
         "annotation-failure.rkt"
         "call-result-key.rkt"
         "define-arity.rkt"
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
  #:namespace-fields
  ([Absolute Path.Absolute]
   [Relative Path.Relative]
   [current_directory current-directory])
  #:properties
  ()
  #:methods
  (bytes
   add
   parts
   string
   to_absolute_path
   only
   add_suffix))

(define/arity #:name Path (path c)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (cond
    [(path? c) c]
    [(bytes? c) (bytes->path c)]
    [(string? c) (string->path c)]
    [else (raise-annotation-failure who c "String || Bytes || Path")]))

(define (path-is-absolute? v)
  (and (path? v)
       (absolute-path? v)))

(define (path-is-relative? v)
  (and (path? v)
       (not (absolute-path? v))))

(define-annotation-syntax Path.Absolute
  (identifier-annotation path-is-absolute? #,(get-path-static-infos)))

(define-annotation-syntax Path.Relative
  (identifier-annotation path-is-relative? #,(get-path-static-infos)))

(define-static-info-syntax current-directory
  (#%function-arity 3)
  (#%call-result #,(get-path-static-infos))
  . #,(get-function-static-infos))

(define/method (Path.bytes s)
  #:primitive (path->bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (bytes->immutable-bytes (path->bytes s)))

(define/method (Path.add p . ss)
  #:primitive (build-path)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (apply build-path p ss))

(define/method (Path.parts p)
  #:primitive (explode-path)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (to-treelist #f (explode-path p)))

(define/method (Path.only p)
  #:primitive (path-only)
  (path-only p))

(define/method (Path.add_suffix p sfx)
  #:primitive (path-add-suffix)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (path-add-suffix p sfx))

(define/method (Path.string s)
  #:primitive (path->string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (path->string s)))

(define/method (Path.to_absolute_path p #:relative_to [base-path (current-directory)])
  #:primitive (path->complete-path)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (path->complete-path p base-path))

(define-annotation-syntax PathString (identifier-annotation path-string? ()))
