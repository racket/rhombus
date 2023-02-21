#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "name-root.rkt"
         "dot-parse.rkt"
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt")

(provide (for-spaces (rhombus/namespace)
                     Bytes))

(module+ for-builtin
  (provide bytes-method-table))

(module+ static-infos
  (provide (for-syntax bytes-static-infos)))

(define-name-root Bytes
  #:fields
  ([length bytes-length]))

(define bytes-method-table
  (hash 'length (method1 bytes-length)))

(define-syntax bytes-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'bytes-length)]
        [else (fail-k)])))))

(define-for-syntax bytes-static-infos
  #'((#%dot-provider bytes-instance)))

(begin-for-syntax
  (install-static-infos! 'bytes bytes-static-infos))
