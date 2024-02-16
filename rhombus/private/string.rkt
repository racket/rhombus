#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/symbol
         racket/keyword
         "provide.rkt"
         "define-operator.rkt"
         (only-in "arithmetic.rkt"
                  ==
                  ===)
         (only-in (submod "print.rkt" for-string)
                  [display rhombus:display]
                  [print rhombus:print])
         "realm.rkt"
         "call-result-key.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "define-arity.rkt"
         (submod "literal.rkt" for-info)
         (submod "annotation.rkt" for-class)
         "mutability.rkt"
         "pack.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "rhombus-primitive.rkt")

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
                    ReadableString
                    ;; temporary:
                    (rename-out [ReadableString StringView])))

(module+ for-builtin
  (provide string-method-table))

(module+ static-infos
  (provide (for-syntax string-static-infos)))

(define-for-syntax any-string-static-infos
  #'((#%index-get String.get)
     (#%append String.append)))

(define-primitive-class ReadableString readable-string
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,any-string-static-infos
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  (;; `to_string` is in "string.rhm"
   #:no-methods)
  #:properties
  ()
  #:methods
  ([length String.length]
   [get String.get]
   [append String.append]
   [substring String.substring]
   [utf8_bytes String.utf8_bytes]
   [latin1_bytes String.latin1_bytes]
   [locale_bytes String.locale_bytes]
   [to_int String.to_int]
   [to_number String.to_number]
   [to_string String.to_string]
   [upcase String.upcase]
   [downcase String.downcase]
   [foldcase String.foldcase]
   [titlecase String.titlecase]
   [normalize_nfd String.normalize_nfd]
   [normalize_nfkd String.normalize_nfkd]
   [normalize_nfc String.normalize_nfc]
   [normalize_nfkc String.normalize_nfkc]
   [grapheme_span String.grapheme_span]
   [grapheme_count String.grapheme_count]))

(define-primitive-class String string
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,any-string-static-infos
  #:existing
  #:opaque
  #:parent #f readable-string
  #:fields ()
  #:namespace-fields
  ([to_string String.to_string]
   [append String.append]
   [length String.length]
   [get String.get]
   [substring String.substring]
   [utf8_bytes String.utf8_bytes]
   [latin1_bytes String.latin1_bytes]
   [locale_bytes String.locale_bytes]
   [to_int String.to_int]
   [to_number String.to_number]
   [to_string String.to_string]
   [upcase String.upcase]
   [downcase String.downcase]
   [foldcase String.foldcase]
   [titlecase String.titlecase]
   [normalize_nfd String.normalize_nfd]
   [normalize_nfkd String.normalize_nfkd]
   [normalize_nfc String.normalize_nfc]
   [normalize_nfkc String.normalize_nfkc]
   [grapheme_span String.grapheme_span]
   [grapheme_count String.grapheme_count])
  #:properties
  ()
  #:methods
  ())

(define-annotation-syntax String (identifier-annotation #'immutable-string? string-static-infos))
(define-annotation-syntax ReadableString (identifier-annotation #'string? readable-string-static-infos))

(define-infix +& append-as-strings
  #:stronger-than (== ===)
  #:static-infos #,string-static-infos)

(define (append-as-strings a b)
  (string-append-immutable (to_string a)
                           (to_string b)))

(define/arity (to_string a #:mode [mode 'text])
  #:static-infos ((#%call-result #,string-static-infos))
  (define (print-to-string a print)
    (define o (open-output-string))
    (print a o)
    (string->immutable-string (get-output-string o)))
  (case mode
    [(text)
     (cond
       [(string? a) (string->immutable-string a)]
       [(symbol? a) (symbol->immutable-string a)]
       [(keyword? a) (keyword->immutable-string a)]
       [(and (syntax? a)
             (let ([t (unpack-term a #f #f)])
               (and (identifier? t)
                    t)))
        => (lambda (t)
             (symbol->immutable-string (syntax-e t)))]
       [else (print-to-string a rhombus:display)])]
    [(expr)
     (print-to-string a rhombus:print)]
    [else
     (raise-argument-error* who rhombus-realm "Any.of(#'text, #'expr)" mode)]))

(set-primitive-contract! 'string? "ReadableString")

(define (check-readable-string who s)
  (unless (string? s)
    (raise-argument-error* who rhombus-realm "ReadableString" s)))

(define/method (String.get s i)
  #:inline
  #:primitive (string-ref)
  (string-ref s i))

(define/method (String.length s)
  #:inline
  #:primitive (string-length)
  (string-length s))

(define/method (String.to_string s)
  #:inline
  #:primitive (string->immutable-string)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string s))

(define/method (String.to_int s)
  (check-readable-string who s)
  (define n (string->number s))
  (and (exact-integer? n)
       n))

(define/method (String.to_number s)
  (check-readable-string who s)
  (string->number s))

(define/method String.substring
  #:inline
  #:primitive (substring)
  #:static-infos ((#%call-result #,string-static-infos))
  (case-lambda
    [(str start) (string->immutable-string (substring str start))]
    [(str start end) (string->immutable-string (substring str start end))]))

(define/method String.append
  #:inline
  #:primitive (string-append-immutable)
  #:static-infos ((#%call-result #,string-static-infos))
  (case-lambda
    [() ""]
    [(s) (string->immutable-string s)]
    [(str1 str2) (string-append-immutable str1 str2)]
    [strs (apply string-append-immutable strs)]))

(define-syntax (define-upcase stx)
  (syntax-parse stx
    [(_ upcase)
     #:do [(define (format-name fmt)
             (datum->syntax #'upcase (string->symbol (format fmt (syntax-e #'upcase)))))]
     #:with method-name (format-name "String.~a")
     #:with fn-name (format-name "string-~a")
     #'(define/method (method-name s)
         #:inline
         #:primitive (fn-name)
         #:static-infos ((#%call-result #,string-static-infos))
         (string->immutable-string (fn-name s)))]))

(define-upcase upcase)
(define-upcase downcase)
(define-upcase titlecase)
(define-upcase foldcase)

(define-syntax (define-normalize stx)
  (syntax-parse stx
    [(_ nfd)
     #:do [(define (format-name fmt)
             (datum->syntax #'nfd (string->symbol (format fmt (syntax-e #'nfd)))))]
     #:with method-name (format-name "String.normalize_~a")
     #:with fn-name (format-name "string-normalize-~a")
     #'(define/method (method-name s)
         #:inline
         #:primitive (fn-name)
         #:static-infos ((#%call-result #,string-static-infos))
         (string->immutable-string (fn-name s)))]))

(define-normalize nfd)
(define-normalize nfkd)
(define-normalize nfc)
(define-normalize nfkc)

(define-syntax (define-bytes stx)
  (syntax-parse stx
    [(_ utf8 utf-8)
     #:with method-name (datum->syntax #'utf8 (string->symbol (format "String.~a_bytes" (syntax-e #'utf8))))
     #:with fn-name (datum->syntax #'utf-8 (string->symbol (format "string->bytes/~a" (syntax-e #'utf-8))))
     #'(define/method method-name
         #:inline
         #:primitive (fn-name)
         #:static-infos ((#%call-result #,indirect-bytes-static-infos))
         (case-lambda
           [(str) (bytes->immutable-bytes (fn-name str))]
           [(str err-byte) (bytes->immutable-bytes (fn-name str err-byte))]
           [(str err-byte start) (bytes->immutable-bytes (fn-name str err-byte start))]
           [(str err-byte start end) (bytes->immutable-bytes (fn-name str err-byte start end))]))]))

(define-bytes utf8 utf-8)
(define-bytes latin1 latin-1)
(define-bytes locale locale)

(define-syntax (define-grapheme stx)
  (syntax-parse stx
    [(_ span)
     #:do [(define (format-name fmt)
             (datum->syntax #'span (string->symbol (format fmt (syntax-e #'span)))))]
     #:with method-name (format-name "String.grapheme_~a")
     #:with fn-name (format-name "string-grapheme-~a")
     #'(define/method method-name
         #:inline
         #:primitive (fn-name)
         (case-lambda
           [(str) (fn-name str)]
           [(str start) (fn-name str start)]
           [(str start end) (fn-name str start end)]))]))

(define-grapheme span)
(define-grapheme count)

(begin-for-syntax
  (install-literal-static-infos! 'string string-static-infos))
