#lang racket/base
(require (for-syntax racket/base)
         racket/symbol
         racket/keyword
         "provide.rkt"
         "define-operator.rkt"
         (only-in "arithmetic.rkt"
                  ==
                  ===)
         (only-in (submod "print.rkt" for-string)
                  [display rhombus:display])
         "realm.rkt"
         "call-result-key.rkt"
         "map-ref-set-key.rkt"
         "define-arity.rkt"
         "name-root.rkt"
         "static-info.rkt"
         "dot-parse.rkt"
         (submod "dot.rkt" for-dot-provider)
         (submod "annotation.rkt" for-class)
         "mutability.rkt")

(provide (for-spaces (#f
                      rhombus/repet)
                     +&)
         (for-spaces (#f
                      rhombus/statinfo)
                     to_string)
         (for-spaces (rhombus/annot
                      rhombus/namespace)
                     String)
         (for-space rhombus/annot
                    StringView))

(module+ for-builtin
  (provide string-method-table))

(module+ static-infos
  (provide (for-syntax string-static-infos)))

(define-for-syntax string-static-infos
  #'((#%dot-provider string-instance)
     (#%map-ref string-ref)
     (#%map-append string-append-immutable)))

(define-annotation-syntax String (identifier-annotation #'immutable-string? string-static-infos))
(define-annotation-syntax StringView (identifier-annotation #'string? string-static-infos))

(define-infix +& append-as-strings
  #:stronger-than (== ===))

(define (append-as-strings a b)
  (string-append-immutable (to_string a)
                           (to_string b)))

(define/arity (to_string a)
  #:static-infos ((#%call-result #,string-static-infos))
  (cond
    [(string? a) (string->immutable-string a)]
    [(symbol? a) (symbol->immutable-string a)]
    [(keyword? a) (keyword->immutable-string a)]
    [(identifier? a) (symbol->immutable-string (syntax-e a))]
    [else
     (define o (open-output-string))
     (rhombus:display a o)
     (string->immutable-string (get-output-string o))]))

(define-name-root String
  #:fields
  ([length string-length]
   to_int
   to_number
   [substring sub_string]
   upcase
   downcase
   foldcase
   titlecase
   normalize_nfd
   normalize_nfkd
   normalize_nfc
   normalize_nfkc
   utf8_bytes
   latin1_bytes
   locale_bytes
   grapheme_span
   grapheme_count))

(define/arity (to_int s)
  (unless (string? s)
    (raise-argument-error* 'to_int rhombus-realm "String" s))
  (define n (string->number s))
  (and (exact-integer? n)
       n))

(define/arity (to_number s)
  (unless (string? s)
    (raise-argument-error* 'to_number rhombus-realm "String" s))
  (string->number s))

(define/arity #:name substring (sub_string str [start 0] [end (and (string? str) (string-length str))])
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (substring str start end)))

(define/arity (upcase s)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (string-upcase s)))
(define/arity (downcase s)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (string-downcase s)))
(define/arity (foldcase s)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (string-foldcase s)))
(define/arity (titlecase s)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (string-titlecase s)))

(define/arity (normalize_nfd s)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (string-normalize-nfd s)))
(define/arity (normalize_nfkd s)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (string-normalize-nfkd s)))
(define/arity (normalize_nfc s)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (string-normalize-nfc s)))
(define/arity (normalize_nfkc s)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (string-normalize-nfkc s)))

(define/arity (utf8_bytes s [err-byte #f] [start 0] [end (and (string? s) (string-length s))])
  #:static-infos ((#%call-result #,indirect-bytes-static-infos))
  (bytes->immutable-bytes (string->bytes/utf-8 s err-byte start end)))
(define/arity (latin1_bytes s [err-byte #f] [start 0] [end (and (string? s) (string-length s))])
  #:static-infos ((#%call-result #,indirect-bytes-static-infos))
  (bytes->immutable-bytes (string->bytes/latin-1 s err-byte start end)))
(define/arity (locale_bytes s [err-byte #f] [start 0] [end (and (string? s) (string-length s))])
  #:static-infos ((#%call-result #,indirect-bytes-static-infos))
  (bytes->immutable-bytes (string->bytes/locale s err-byte start end)))

(define/arity (grapheme_span s [start 0] [end (and (string? s) (string-length s))])
  (string-grapheme-span s start end))
(define/arity (grapheme_count s [start 0] [end (and (string? s) (string-length s))])
  (string-grapheme-count s start end))

(define string-method-table
  (hash 'length (method1 string-length)
        'to_int (method1 to_int)
        'to_number (method1 to_number)
        'substring (lambda (str)
                     (lambda (start [end (and (string? str) (string-length str))])
                       (string->immutable-string (substring str start end))))
        'upcase (method1 upcase)
        'downcase (method1 downcase)
        'foldcase (method1 foldcase)
        'titlecase (method1 titlecase)
        'normalize_nfd (method1 normalize_nfd)
        'normalize_nfkd (method1 normalize_nfkd)
        'normalize_nfc (method1 normalize_nfc)
        'normalize_nfkc (method1 normalize_nfkc)
        'utf8_bytes (lambda (str)
                      (lambda ([err-byte #f] [start 0] [end (and (string? str) (string-length str))])
                        (bytes->immutable-bytes (string->bytes/utf-8 str err-byte start end))))
        'latin1_bytes (lambda (str)
                        (lambda ([err-byte #f] [start 0] [end (and (string? str) (string-length str))])
                          (bytes->immutable-bytes (string->bytes/latin-1 str err-byte start end))))
        'locale_bytes (lambda (str)
                        (lambda ([err-byte #f] [start 0] [end (and (string? str) (string-length str))])
                          (bytes->immutable-bytes (string->bytes/locale str err-byte start end))))
        'grapheme_span (lambda (str)
                         (lambda (start [end (and (string? str) (string-length str))])
                           (string-grapheme-span str start end)))
        'grapheme_count (lambda (str)
                          (lambda (start [end (and (string? str) (string-length str))])
                            (string-grapheme-count str start end)))))

(define-syntax string-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'string-length)]
        [(to_int) (0ary #'to_int)]
        [(to_number) (0ary #'to_number)]
        [(substring) (nary #'sub_string 6 #'sub_string string-static-infos)]
        [(upcase) (0ary #'upcase)]
        [(downcase) (0ary #'downcase)]
        [(foldcase) (0ary #'foldcase)]
        [(titlecase) (0ary #'titlecase)]
        [(normalize_nfd) (0ary #'normalize_nfd)]
        [(normalize_nfkd) (0ary #'normalize_nfkd)]
        [(normalize_nfc) (0ary #'normalize_nfc)]
        [(normalize_nfkc) (0ary #'normalize_nfkc)]
        [(utf8_bytes) (nary #'utf8_bytes 15 #'utf8_bytes indirect-bytes-static-infos)]
        [(latin1_bytes) (nary #'utf8_bytes 15 #'latin1_bytes indirect-bytes-static-infos)]
        [(locale_bytes) (nary #'utf8_bytes 15 #'locale_bytes indirect-bytes-static-infos)]
        [(grapheme_span) (nary #'grapheme_span 6 #'grapheme_span)]
        [(grapheme_count) (nary #'grapheme_count 6 #'grapheme_count)]
        [else (fail-k)])))))

(begin-for-syntax
  (install-static-infos! 'string string-static-infos))
