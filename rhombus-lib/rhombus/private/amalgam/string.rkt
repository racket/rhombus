#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (only-in racket/string string-contains?)
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
         "index-result-key.rkt"
         "append-key.rkt"
         "compare-key.rkt"
         "sequence-constructor-key.rkt"
         "define-arity.rkt"
         (submod "literal.rkt" for-info)
         (submod "annotation.rkt" for-class)
         (submod "char.rkt" for-static-info)
         "mutability.rkt"
         "pack.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "rhombus-primitive.rkt"
         "number.rkt"
         "static-info.rkt")

(provide (for-spaces (#f
                      rhombus/repet)
                     +&)
         (for-spaces (#f
                      rhombus/statinfo)
                     to_string
                     repr)
         (for-spaces (rhombus/annot
                      rhombus/namespace)
                     String)
         (for-space rhombus/annot
                    ReadableString
                    StringCI
                    ReadableStringCI))

(module+ for-builtin
  (provide string-method-table))

(module+ static-infos
  (provide (for-syntax get-string-static-infos)))

(define-static-info-getter get-any-string-static-infos
  (#%index-get String.get)
  (#%index-result #,(get-char-static-infos)) ;; needed by `for` sequence, for example
  (#%append String.append)
  (#%sequence-constructor String.to_sequence/optimize)
  (#%compare ((< string<?)
              (<= string<=?)
              (= string=?)
              (!= string!=?)
              (>= string>=?)
              (> string>?))))

(define-primitive-class ReadableString readable-string
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-string-static-infos)
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
   [contains String.contains]
   [append String.append]
   [substring String.substring]
   [make String.make]
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
   [grapheme_count String.grapheme_count]
   [to_sequence String.to_sequence]
   [copy String.copy]
   [snapshot String.snapshot]))

(define-primitive-class String string
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-string-static-infos)
  #:existing
  #:opaque
  #:parent #f readable-string
  #:fields ()
  #:namespace-fields
  ([to_string String.to_string]
   [append String.append]
   [length String.length]
   [get String.get]
   [contains String.contains]
   [substring String.substring]
   [make String.make]
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
   [grapheme_count String.grapheme_count]
   [to_sequence String.to_sequence]
   [copy String.copy]
   [snapshot String.snapshot])
  #:properties
  ()
  #:methods
  ())

(define-annotation-syntax String (identifier-annotation immutable-string? #,(get-string-static-infos)))
(define-annotation-syntax ReadableString (identifier-annotation string? #,(get-readable-string-static-infos)))

(define-values-for-syntax (get-string-ci-static-infos
                           get-readable-string-ci-static-infos)
  (let ([convert
         (lambda (get-sis)
           (lambda ()
             (for/list ([si (in-list (get-sis))])
               (syntax-parse si
                 #:datum-literals (#%compare)
                 [(#%compare . _) #'(#%compare ((< string-ci<?)
                                                (<= string-ci<=?)
                                                (= string-ci=?)
                                                (!= string-ci!=?)
                                                (>= string-ci>=?)
                                                (> string-ci>?)))]
                 [_ si]))))])
    (values (convert get-string-static-infos)
            (convert get-readable-string-static-infos))))

(define-annotation-syntax StringCI (identifier-annotation immutable-string? #,(get-string-ci-static-infos) #:static-only))
(define-annotation-syntax ReadableStringCI (identifier-annotation string? #,(get-readable-string-ci-static-infos) #:static-only))

(define-infix +& append-as-strings
  #:stronger-than (== ===)
  #:static-infos #,(get-string-static-infos))

(define (append-as-strings a b)
  (string-append-immutable (to_string a)
                           (to_string b)))

(define/arity (to_string a #:mode [mode 'text])
  #:static-infos ((#%call-result #,(get-string-static-infos)))
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
     (raise-argument-error* who rhombus-realm "PrintMode" mode)]))

(define/arity (repr a)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (to_string a #:mode 'expr))

(set-primitive-contract! 'string? "ReadableString")

(define (check-readable-string who s)
  (unless (string? s)
    (raise-argument-error* who rhombus-realm "ReadableString" s)))

(define/method (String.get s i)
  #:inline
  #:primitive (string-ref)
  #:static-infos ((#%call-result #,(get-char-static-infos)))
  (string-ref s i))

(define/method (String.length s)
  #:inline
  #:primitive (string-length)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (string-length s))

(define/method (String.to_string s)
  #:inline
  #:primitive (string->immutable-string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string s))

(define/method (String.to_int s)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (check-readable-string who s)
  (define n (string->number s))
  (and (exact-integer? n)
       n))

(define/method (String.to_number s)
  #:static-infos ((#%call-result #,(get-number-static-infos)))
  (check-readable-string who s)
  (string->number s))

(define/method (String.contains s1 s2)
  (check-readable-string who s1)
  (check-readable-string who s2)
  (string-contains? s1 s2))

(define/method (String.copy s)
  #:primitive (string-copy)
  #:static-infos ((#%call-result #,(get-readable-string-static-infos)))
  (string-copy s))

(define/method (String.snapshot s)
  #:primitive (string->immutable-string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string s))

(define/method (String.make n c)
  #:primitive (make-string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (make-string n c)))

(define/method String.substring
  #:inline
  #:primitive (substring)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (case-lambda
    [(str start) (string->immutable-string (substring str start))]
    [(str start end) (string->immutable-string (substring str start end))]))

(define/method String.append
  #:inline
  #:primitive (string-append-immutable)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
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
         #:static-infos ((#%call-result #,(get-string-static-infos)))
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
         #:static-infos ((#%call-result #,(get-string-static-infos)))
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
         #:static-infos ((#%call-result #,(indirect-get-bytes-static-infos)))
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

(define-sequence-syntax String.to_sequence/optimize
  (lambda () #'String.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ str-expr)]
       #`[(id) (in-string #,(discard-static-infos #'str-expr))]]
      [_ #f])))

(define/method (String.to_sequence str)
  #:inline
  #:primitive (in-string)
  #:static-infos ((#%call-result ((#%sequence-constructor #t))))
  (in-string str))

(define (string!=? a b)
  (if (and (string? a) (string? b))
      (not (string=? a b))
      (raise-argument-error* '!= rhombus-realm "String" (if (string? a) b a))))

(define (string-ci!=? a b)
  (if (and (string? a) (string? b))
      (not (string-ci=? a b))
      (raise-argument-error* '!= rhombus-realm "String" (if (string? a) b a))))

(begin-for-syntax
  (install-get-literal-static-infos! 'string get-string-static-infos))
