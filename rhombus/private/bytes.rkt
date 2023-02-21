#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "name-root.rkt"
         "dot-parse.rkt"
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "define-arity.rkt"
         (submod "annotation.rkt" for-class))

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Bytes))

(module+ for-builtin
  (provide bytes-method-table))

(module+ static-infos
  (provide (for-syntax bytes-static-infos)))

(define-for-syntax bytes-static-infos
  #'((#%dot-provider bytes-instance)))

(define-annotation-syntax Bytes (identifier-annotation #'bytes? bytes-static-infos))

(define-name-root Bytes
  #:fields
  ([length bytes-length]
   [subbytes sub_bytes]
   utf8_string
   latin1_string
   locale_string))

(define/arity #:name subbytes (sub_bytes str [start 0] [end (and (bytes? str) (bytes-length str))])
  #:static-infos ((#%call-result #,bytes-static-infos))
  (bytes->immutable-bytes (subbytes str start end)))

(define/arity (utf8_string s [err-char #f] [start 0] [end (and (bytes? s) (bytes-length s))])
  #:static-infos ((#%call-result #,indirect-string-static-infos))
  (string->immutable-string (bytes->string/utf-8 s err-char start end)))
(define/arity (latin1_string s [err-char #f] [start 0] [end (and (bytes? s) (bytes-length s))])
  #:static-infos ((#%call-result #,indirect-string-static-infos))
  (string->immutable-string (bytes->string/latin-1 s err-char start end)))
(define/arity (locale_string s [err-char #f] [start 0] [end (and (bytes? s) (bytes-length s))])
  #:static-infos ((#%call-result #,indirect-string-static-infos))
  (string->immutable-string (bytes->string/locale s err-char start end)))

(define bytes-method-table
  (hash 'length (method1 bytes-length)
        'subbytes (lambda (str)
                    (lambda (start [end (and (bytes? str) (bytes-length str))])
                      (bytes->immutable-bytes (subbytes str start end))))
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
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'bytes-length)]
        [(subbytes) (nary #'sub_bytes 6 #'sub_bytes bytes-static-infos)]
        [(utf8_string) (nary #'utf8_string 15 #'utf8_string indirect-string-static-infos)]
        [(latin1_string) (nary #'utf8_string 15 #'latin1_string indirect-string-static-infos)]
        [(locale_string) (nary #'utf8_string 15 #'locale_string indirect-string-static-infos)]
        [else (fail-k)])))))

(begin-for-syntax
  (install-static-infos! 'bytes bytes-static-infos))
