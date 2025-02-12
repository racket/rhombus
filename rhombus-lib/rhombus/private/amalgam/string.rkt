#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     "make-get-veneer-like-static-infos.rkt")
         racket/string
         racket/symbol
         racket/keyword
         racket/unsafe/undefined
         "../version-case.rkt"
         "provide.rkt"
         "define-operator.rkt"
         (only-in (submod "print.rkt" for-string)
                  [display rhombus:display]
                  [print rhombus:print])
         "call-result-key.rkt"
         "index-key.rkt"
         "index-result-key.rkt"
         "append-key.rkt"
         "compare-key.rkt"
         "sequence-constructor-key.rkt"
         "maybe-key.rkt"
         "realm.rkt"
         "define-arity.rkt"
         "binding.rkt"
         (submod "literal.rkt" for-info)
         (submod "annotation.rkt" for-class)
         (submod "char.rkt" for-static-info)
         (submod "list.rkt" for-compound-repetition)
         "mutability.rkt"
         "pack.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "number.rkt"
         "treelist.rkt"
         "static-info.rkt"
         "rx-object.rkt"
         "order-primitive.rkt"
         (submod "range.rkt" for-substring))

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
                    MutableString
                    StringCI
                    ReadableStringCI
                    StringLocale
                    ReadableStringLocale
                    StringLocaleCI
                    ReadableStringLocaleCI))

(module+ for-builtin
  (provide string-method-table))

(module+ static-infos
  (provide (for-syntax get-string-static-infos)))

(define-static-info-getter get-any-string-static-infos
  (#%index-get String.get)
  (#%index-result #,(get-char-static-infos))
  (#%append String.append)
  (#%sequence-constructor String.to_sequence/optimize)
  (#%compare ((< string<?)
              (<= string<=?)
              (= string=?)
              (!= string!=?)
              (>= string>=?)
              (> string>?))))

(define-primitive-class ReadableString readable-string string
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-string-static-infos)
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  (;; `to_string` is in "string.rhm"
   #:no-methods)
  #:properties
  ()
  #:methods
  ()
  #:dot-methods
  ([length String.length]
   [get String.get]
   [find String.find]
   [contains String.contains]
   [starts_with String.starts_with]
   [ends_with String.ends_with]
   [append String.append]
   [substring String.substring]
   [trim String.trim]
   [split String.split]
   [replace String.replace]
   [utf8_bytes String.utf8_bytes]
   [latin1_bytes String.latin1_bytes]
   [locale_bytes String.locale_bytes]
   [maybe_to_int String.maybe_to_int]
   [maybe_to_number String.maybe_to_number]
   [to_int String.to_int]
   [to_number String.to_number]
   [to_string String.to_string]
   [upcase String.upcase]
   [downcase String.downcase]
   [foldcase String.foldcase]
   [titlecase String.titlecase]
   [locale_upcase String.locale_upcase]
   [locale_downcase String.locale_downcase]
   [normalize_nfd String.normalize_nfd]
   [normalize_nfkd String.normalize_nfkd]
   [normalize_nfc String.normalize_nfc]
   [normalize_nfkc String.normalize_nfkc]
   [grapheme_span String.grapheme_span]
   [grapheme_count String.grapheme_count]
   [to_sequence String.to_sequence]
   [copy String.copy]
   [snapshot String.snapshot]))

(define-primitive-class String string immutable-string
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-string-static-infos)
  #:existing
  #:just-annot #:no-primitive
  #:parent #f readable-string
  #:fields ()
  #:namespace-fields
  ([to_string String.to_string]
   [append String.append]
   [length String.length]
   [get String.get]
   [find String.find]
   [contains String.contains]
   [starts_with String.starts_with]
   [ends_with String.ends_with]
   [substring String.substring]
   [trim String.trim]
   [split String.split]
   [join String.join]
   [replace String.replace]
   [make String.make]
   [utf8_bytes String.utf8_bytes]
   [latin1_bytes String.latin1_bytes]
   [locale_bytes String.locale_bytes]
   [maybe_to_int String.maybe_to_int]
   [maybe_to_number String.maybe_to_number]
   [to_int String.to_int]
   [to_number String.to_number]
   [to_string String.to_string]
   [upcase String.upcase]
   [downcase String.downcase]
   [foldcase String.foldcase]
   [titlecase String.titlecase]
   [locale_upcase String.locale_upcase]
   [locale_downcase String.locale_downcase]
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

(define-for-syntax (convert-string-ci-compare-static-info static-info)
  (syntax-parse static-info
    #:datum-literals (#%compare)
    [(#%compare . _) #'(#%compare ((< string-ci<?)
                                   (<= string-ci<=?)
                                   (= string-ci=?)
                                   (!= string-ci!=?)
                                   (>= string-ci>=?)
                                   (> string-ci>?)))]
    [_ static-info]))

(define-for-syntax (get-string-ci-static-infos)
  (make-get-veneer-like-static-infos get-string-static-infos
                                     convert-string-ci-compare-static-info))

(define-for-syntax (get-readable-string-ci-static-infos)
  (make-get-veneer-like-static-infos get-readable-string-static-infos
                                     convert-string-ci-compare-static-info))

(define-annotation-syntax MutableString
  (identifier-annotation mutable-string? #,(get-readable-string-static-infos)))

(define-annotation-syntax StringCI
  (identifier-annotation immutable-string? #,(get-string-ci-static-infos) #:static-only))
(define-annotation-syntax ReadableStringCI
  (identifier-annotation string? #,(get-readable-string-ci-static-infos) #:static-only))

(define-for-syntax (convert-string-locale-compare-static-info static-info)
  (syntax-parse static-info
    #:datum-literals (#%compare)
    [(#%compare . _) #'(#%compare ((< string-locale<?)
                                   (<= string-locale<=?)
                                   (= string-locale=?)
                                   (!= string-locale!=?)
                                   (>= string-locale>=?)
                                   (> string-locale>?)))]
    [_ static-info]))

(define-for-syntax (get-string-locale-static-infos)
  (make-get-veneer-like-static-infos get-string-static-infos
                                     convert-string-locale-compare-static-info))

(define-for-syntax (get-readable-string-locale-static-infos)
  (make-get-veneer-like-static-infos get-readable-string-static-infos
                                     convert-string-locale-compare-static-info))

(define-for-syntax (convert-string-locale-ci-compare-static-info static-info)
  (syntax-parse static-info
    #:datum-literals (#%compare)
    [(#%compare . _) #'(#%compare ((< string-locale-ci<?)
                                   (<= string-locale-ci<=?)
                                   (= string-locale-ci=?)
                                   (!= string-locale-ci!=?)
                                   (>= string-locale-ci>=?)
                                   (> string-locale-ci>?)))]
    [_ static-info]))

(define-for-syntax (get-string-locale-ci-static-infos)
  (make-get-veneer-like-static-infos get-string-static-infos
                                     convert-string-locale-compare-static-info))

(define-for-syntax (get-readable-string-locale-ci-static-infos)
  (make-get-veneer-like-static-infos get-readable-string-static-infos
                                     convert-string-locale-ci-compare-static-info))

(define-annotation-syntax StringLocale
  (identifier-annotation immutable-string? #,(get-string-locale-static-infos) #:static-only))
(define-annotation-syntax ReadableStringLocale
  (identifier-annotation string? #,(get-readable-string-locale-static-infos) #:static-only))
(define-annotation-syntax StringLocaleCI
  (identifier-annotation immutable-string? #,(get-string-locale-ci-static-infos) #:static-only))
(define-annotation-syntax ReadableStringLocaleCI
  (identifier-annotation string? #,(get-readable-string-locale-ci-static-infos) #:static-only))

(define-infix +& append-as-strings
  #:order concatenation
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
     (raise-annotation-failure who mode "PrintMode")]))

(define/arity (repr a)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (to_string a #:mode 'expr))

(define (check-readable-string who s)
  (unless (string? s)
    (raise-annotation-failure who s "ReadableString")))

(define/method (String.get s i)
  #:primitive (string-ref)
  #:static-infos ((#%call-result #,(get-char-static-infos)))
  (string-ref s i))

(define/method (String.length s)
  #:primitive (string-length)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (string-length s))

(define/method (String.to_string s)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (check-readable-string who s)
  (string->immutable-string s))

;; FIXME these should use shrubbery lexeme syntax
(define (rhombus-string->int s)
  (define n (string->number s))
  (and (exact-integer? n)
       n))

(define (rhombus-string->number s)
  (string->number s))

(define/method (String.maybe_to_int s)
  #:static-infos ((#%call-result ((#%maybe #,(get-int-static-infos)))))
  (check-readable-string who s)
  (rhombus-string->int s))

(define/method (String.maybe_to_number s)
  #:static-infos ((#%call-result ((#%maybe #,(get-number-static-infos)))))
  (check-readable-string who s)
  (rhombus-string->number s))

(define/method (String.to_int s)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (check-readable-string who s)
  (or (rhombus-string->int s)
      (raise-arguments-error* who rhombus-realm "string does not parse as an integer"
                              "string" s)))

(define/method (String.to_number s)
  #:static-infos ((#%call-result #,(get-number-static-infos)))
  (check-readable-string who s)
  (or (rhombus-string->number s)
      (raise-arguments-error* who rhombus-realm "string does not parse as a number"
                              "string" s)))

(define-annotation-syntax String.to_int
  (make-identifier-binding-annotation
   (lambda (stx)
     (values (binding-form #'from-string-infoer #`(x rhombus-string->int #,(shrubbery-syntax->string stx)))
             #'x
             (get-int-static-infos)))))

(define-annotation-syntax String.to_number
  (make-identifier-binding-annotation
   (lambda (stx)
     (values (binding-form #'from-string-infoer #`(x rhombus-string->number  #,(shrubbery-syntax->string stx)))
             #'x
             (get-real-static-infos)))))

(define-syntax (from-string-infoer stx)
  (syntax-parse stx
    [(_ static-infos (x cvt ann-str))
     (binding-info (syntax-e #'ann-str)
                   #'s
                   #'()
                   #'((x (0)))
                   #'from-string-matcher
                   #'(v)
                   #'from-string-committer
                   #'from-string-binder
                   #'(x v cvt))]))

(define-syntax (from-string-matcher stx)
  (syntax-parse stx
    [(_ arg-id (x v cvt) IF success fail)
     #'(begin
         (define v (and (immutable-string? arg-id) (cvt arg-id)))
         (IF v success fail))]))

(define-syntax (from-string-committer stx)
  (syntax-parse stx
    [(_ arg-id (v) _)
     #'(begin)]))

(define-syntax (from-string-binder stx)
  (syntax-parse stx
    [(_ arg-id (v) (x . _))
     #'(define x v)]))

(define/method (String.find s1 s2)
  #:static-infos ((#%call-result ((#%maybe #,(get-int-static-infos)))))
  (check-readable-string who s1)
  (check-readable-string who s2)
  (meta-if-version-at-least
   "8.15.0.7"
   (string-find s1 s2)
   (and (string-contains? s1 s2)
        ;; find position the slow way:
        (for/or ([i (in-range 0 (string-length s1))])
          (and (string=? (substring s1 i (+ i (string-length s2))) s2) i)))))

(define/method (String.contains s1 s2)
  (check-readable-string who s1)
  (check-readable-string who s2)
  (string-contains? s1 s2))

(define/method (String.starts_with s1 s2)
  (check-readable-string who s1)
  (check-readable-string who s2)
  (string-prefix? s1 s2))

(define/method (String.ends_with s1 s2)
  (check-readable-string who s1)
  (check-readable-string who s2)
  (string-suffix? s1 s2))

(define/method (String.trim s1 [sep unsafe-undefined]
                            #:start [start? #t]
                            #:end [end? #t]
                            #:repeat [repeat? #f])
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (check-readable-string who s1)
  (string->immutable-string
   (cond
     [(eq? sep unsafe-undefined)
      (string-trim s1 #:left? start? #:right? end? #:repeat? repeat?)]
     [else
      (unless (or (string? sep) (rx? sep))
        (raise-annotation-failure who sep "ReadableString || RX"))
      (string-trim s1 (if (string? sep) sep (rx-regexp sep))
                   #:left? start? #:right? end? #:repeat? repeat?)])))

(define/method (String.split s1 [sep unsafe-undefined]
                             #:trim [trim? #t]
                             #:repeat [repeat? #f])
  #:static-infos ((#%call-result ((#%index-result #,(get-string-static-infos))
                                  #,@(get-treelist-static-infos))))
  (check-readable-string who s1)
  (define l
    (cond
      [(eq? sep unsafe-undefined)
       (string-split s1 #:trim? trim? #:repeat? repeat?)]
      [else
       (unless (or (string? sep) (rx? sep))
         (raise-annotation-failure who sep "ReadableString || RX"))
       (string-split s1 (if (string? sep) sep (rx-regexp sep))
                     #:trim? trim? #:repeat? repeat?)]))
  (for/treelist ([s (in-list l)])
    (string->immutable-string s)))

(define/method (String.join strs [sep " "]
                            #:before_last [before_last sep])
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (unless (and (treelist? strs)
               (for/and ([s (in-treelist strs)])
                 (string? s)))
    (raise-annotation-failure who strs "List.of(ReadableString)"))
  (check-readable-string who sep)
  (check-readable-string who before_last)
  (string->immutable-string
   (string-join (treelist->list strs) sep
                #:before-last before_last)))

(define/method (String.replace s1 from to
                               #:all [all? #f])
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (check-readable-string who s1)
  (unless (or (string? from) (rx? from))
    (raise-annotation-failure who from "ReadableString || RX"))
  (check-readable-string who to)
  (string->immutable-string
   (string-replace s1 from to #:all? all?)))

(define/method (String.copy s)
  #:primitive (string-copy)
  #:static-infos ((#%call-result #,(get-readable-string-static-infos)))
  (string-copy s))

(define/method (String.snapshot s)
  #:primitive (string->immutable-string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string s))

(define/arity (String.make n c)
  #:primitive (make-string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (make-string n c)))

(define (substring/range who str r)
  (check-readable-string who str)
  (define-values (start end)
    (range-canonical-start+end who "string" r str 0 (string-length str)))
  (substring str start end))

(define/method String.substring
  #:primitive (substring)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (case-lambda
    [(str r) (string->immutable-string (substring/range who str r))]
    [(str start end) (string->immutable-string (substring str start end))]))

(define/method String.append
  #:primitive (string-append-immutable)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (case-lambda
    [() ""]
    [(s) (string-append-immutable s)]
    [(s1 s2) (string-append-immutable s1 s2)]
    [(s1 s2 s3) (string-append-immutable s1 s2 s3)]
    [strs (apply string-append-immutable strs)]))

(define-syntax (define-upcase stx)
  (syntax-parse stx
    [(_ upcase (~optional rkt-upcase))
     #:do [(define (format-name fmt upcase-stx)
             (datum->syntax upcase-stx (string->symbol (format fmt (syntax-e upcase-stx)))))]
     #:with method-name (format-name "String.~a" #'upcase)
     #:with fn-name (format-name "string-~a" (or (attribute rkt-upcase) #'upcase))
     #'(define/method (method-name s)
         #:primitive (fn-name)
         #:static-infos ((#%call-result #,(get-string-static-infos)))
         (string->immutable-string (fn-name s)))]))

(define-upcase upcase)
(define-upcase downcase)
(define-upcase titlecase)
(define-upcase foldcase)
(define-upcase locale_upcase locale-upcase)
(define-upcase locale_downcase locale-downcase)

(define-syntax (define-normalize stx)
  (syntax-parse stx
    [(_ nfd)
     #:do [(define (format-name fmt)
             (datum->syntax #'nfd (string->symbol (format fmt (syntax-e #'nfd)))))]
     #:with method-name (format-name "String.normalize_~a")
     #:with fn-name (format-name "string-normalize-~a")
     #'(define/method (method-name s)
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
  #:primitive (in-string)
  #:static-infos ((#%call-result ((#%sequence-constructor #t))))
  (in-string str))

(define (raise-string-comp-failure who a b)
  (raise-annotation-failure '!= (if (string? a) b a) "ReadableString"))

(define (string!=? a b)
  (if (and (string? a) (string? b))
      (not (string=? a b))
      (raise-string-comp-failure '!= a b)))

(define (string-ci!=? a b)
  (if (and (string? a) (string? b))
      (not (string-ci=? a b))
      (raise-string-comp-failure '!= a b)))

(define (string-locale!=? a b)
  (if (and (string? a) (string? b))
      (not (string-locale=? a b))
      (raise-string-comp-failure '!= a b)))

(define (string-locale<=? a b)
  (if (and (string? a) (string? b))
      (not (string-locale>? a b))
      (raise-string-comp-failure '!= a b)))

(define (string-locale>=? a b)
  (if (and (string? a) (string? b))
      (not (string-locale<? a b))
      (raise-string-comp-failure '!= a b)))

(define (string-locale-ci!=? a b)
  (if (and (string? a) (string? b))
      (not (string-locale-ci=? a b))
      (raise-string-comp-failure '!= a b)))

(define (string-locale-ci<=? a b)
  (if (and (string? a) (string? b))
      (not (string-locale-ci>? a b))
      (raise-string-comp-failure '!= a b)))

(define (string-locale-ci>=? a b)
  (if (and (string? a) (string? b))
      (not (string-locale-ci<? a b))
      (raise-string-comp-failure '!= a b)))

(begin-for-syntax
  (install-get-literal-static-infos! 'string get-string-static-infos))
