#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "name-root.rkt"
         "dot-parse.rkt"
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "define-arity.rkt"
         "call-result-key.rkt"
         "map-ref-set-key.rkt"
         (submod "annotation.rkt" for-class)
         "mutability.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Bytes)
         (for-space rhombus/annot
                    MutableBytes
                    ImmutableBytes))

(module+ for-builtin
  (provide bytes-method-table))

(module+ static-infos
  (provide (for-syntax bytes-static-infos)))

(define-for-syntax bytes-static-infos
  #'((#%dot-provider bytes-instance)
     (#%map-ref bytes-ref)
     (#%map-set! bytes-set!)
     (#%map-append bytes-append)))

(define-annotation-syntax Bytes (identifier-annotation #'bytes? bytes-static-infos))
(define-annotation-syntax MutableBytes (identifier-annotation #'mutable-bytes? bytes-static-infos))
(define-annotation-syntax ImmutableBytes (identifier-annotation #'immutable-bytes? bytes-static-infos))

(define-name-root Bytes
  #:fields
  ([make make-bytes]
   [length bytes-length]
   [subbytes sub_bytes]
   copy
   copy_from
   utf8_string
   latin1_string
   locale_string))

(define/arity #:name subbytes (sub_bytes str [start 0] [end (and (bytes? str) (bytes-length str))])
  #:static-infos ((#%call-result #,bytes-static-infos))
  (subbytes str start end))

(define/arity (utf8_string s [err-char #f] [start 0] [end (and (bytes? s) (bytes-length s))])
  #:static-infos ((#%call-result #,indirect-string-static-infos))
  (string->immutable-string (bytes->string/utf-8 s err-char start end)))
(define/arity (latin1_string s [err-char #f] [start 0] [end (and (bytes? s) (bytes-length s))])
  #:static-infos ((#%call-result #,indirect-string-static-infos))
  (string->immutable-string (bytes->string/latin-1 s err-char start end)))
(define/arity (locale_string s [err-char #f] [start 0] [end (and (bytes? s) (bytes-length s))])
  #:static-infos ((#%call-result #,indirect-string-static-infos))
  (string->immutable-string (bytes->string/locale s err-char start end)))
(define/arity (copy s)
  #:static-infos ((#%call-result #,bytes-static-infos))
  (bytes-copy s))
(define/arity (copy_from s dest-start src [src-start 0] [src-end (and (bytes? src) (bytes-length src))])
  #:static-infos ((#%call-result #,bytes-static-infos))
  (bytes-copy! s dest-start src src-start src-end))

(define bytes-method-table
  (hash 'length (method1 bytes-length)
        'subbytes (lambda (str)
                    (lambda (start [end (and (bytes? str) (bytes-length str))])
                      (subbytes str start end)))
        'copy (method1 bytes-copy)
        'copy_from (lambda (str)
                     (lambda (dest-start src [src-start 0] [src-end (and (bytes? src) (bytes-length src))])
                       (bytes-copy! str dest-start src src-start src-end)))
        'utf8_string (lambda (str)
                       (lambda ([err-char #f] [start 0] [end (and (bytes? str) (bytes-length str))])
                         (string->immutable-string (bytes->string/utf-8 str err-char start end))))
        'latin1_string (lambda (str)
                         (lambda ([err-char #f] [start 0] [end (and (bytes? str) (bytes-length str))])
                           (string->immutable-string (bytes->string/latin-1 str err-char start end))))
        'locale_string (lambda (str)
                         (lambda ([err-char #f] [start 0] [end (and (bytes? str) (bytes-length str))])
                           (string->immutable-string (bytes->string/locale str err-char start end))))))

(define-syntax bytes-instance
  (dot-provider
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'bytes-length)]
        [(subbytes) (nary #'sub_bytes 6 #'sub_bytes bytes-static-infos)]
        [(copy) (0ary #'copy bytes-static-infos)]
        [(copy_from) (nary #'copy_from 28 #'copy_from #'())]
        [(utf8_string) (nary #'utf8_string 15 #'utf8_string indirect-string-static-infos)]
        [(latin1_string) (nary #'utf8_string 15 #'latin1_string indirect-string-static-infos)]
        [(locale_string) (nary #'utf8_string 15 #'locale_string indirect-string-static-infos)]
        [else (fail-k)])))))

(begin-for-syntax
  (install-static-infos! 'bytes bytes-static-infos))
