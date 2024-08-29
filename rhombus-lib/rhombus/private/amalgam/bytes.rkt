#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         (submod "literal.rkt" for-info)
         "define-arity.rkt"
         "call-result-key.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "compare-key.rkt"
         "sequence-constructor-key.rkt"
         (submod "annotation.rkt" for-class)
         "mutability.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "rhombus-primitive.rkt"
         "number.rkt"
         "realm.rkt"
         "static-info.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Bytes)
         (for-space rhombus/annot
                    MutableBytes
                    ImmutableBytes))

(module+ for-builtin
  (provide bytes-method-table))

(module+ static-infos
  (provide (for-syntax get-bytes-static-infos)))

(define-primitive-class Bytes bytes
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%index-get Bytes.get)
                          (#%index-set Bytes.set)
                          (#%append Bytes.append)
                          (#%sequence-constructor Bytes.to_sequence/optimize)
                          (#%compare ((< bytes<?)
                                      (<= bytes<=?)
                                      (= bytes=?)
                                      (!= bytes!=?)
                                      (>= bytes>=?)
                                      (> bytes>?))))
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  ([make Bytes.make])
  #:properties
  ()
  #:methods
  (length
   get
   set
   append
   subbytes
   copy
   copy_from
   utf8_string
   latin1_string
   locale_string
   to_sequence
   snapshot))

(void (set-primitive-subcontract! '(bytes? (not/c immutable?)) 'mutable-bytes?))
(void (set-primitive-contract! 'mutable-bytes? "MutableBytes"))
(define-annotation-syntax MutableBytes (identifier-annotation mutable-bytes? #,(get-bytes-static-infos)))
(define-annotation-syntax ImmutableBytes (identifier-annotation immutable-bytes? #,(get-bytes-static-infos)))

(define/method (Bytes.get b i)
  #:primitive (bytes-ref)
  (bytes-ref b i))

(define/method (Bytes.set b i x)
  #:primitive (bytes-set!)
  (bytes-set! b i x))

(define/method Bytes.append
  #:primitive (bytes-append)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (case-lambda
    [() (bytes)]
    [(b1) (bytes-append b1)]
    [(b1 b2) (bytes-append b1 b2)]
    [(b1 b2 b3) (bytes-append b1 b2 b3)]
    [args (apply bytes-append args)]))

(define/arity Bytes.make
  #:primitive (make-bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (case-lambda
    [(len) (make-bytes len)]
    [(len val) (make-bytes len val)]))

(define/method (Bytes.length bstr)
  #:primitive (bytes-length)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (bytes-length bstr))

(define/method Bytes.subbytes
  #:primitive (subbytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (case-lambda
    [(bstr start) (subbytes bstr start)]
    [(bstr start end) (subbytes bstr start end)]))

(define-syntax (define-string stx)
  (syntax-parse stx
    [(_ utf8 utf-8)
     #:with method-name (datum->syntax #'utf8 (string->symbol (format "Bytes.~a_string" (syntax-e #'utf8))))
     #:with fn-name (datum->syntax #'utf-8 (string->symbol (format "bytes->string/~a" (syntax-e #'utf-8))))
     #'(define/method method-name
         #:primitive (fn-name)
         #:static-infos ((#%call-result #,(indirect-get-string-static-infos)))
         (case-lambda
           [(bstr) (string->immutable-string (fn-name bstr))]
           [(bstr err-char) (string->immutable-string (fn-name bstr err-char))]
           [(bstr err-char start) (string->immutable-string (fn-name bstr err-char start))]
           [(bstr err-char start end) (string->immutable-string (fn-name bstr err-char start end))]))]))

(define-string utf8 utf-8)
(define-string latin1 latin-1)
(define-string locale locale)

(define/method (Bytes.copy bstr)
  #:primitive (bytes-copy)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (bytes-copy bstr))

(define/method Bytes.copy_from
  #:primitive (bytes-copy!)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (case-lambda
    [(bstr dest-start src) (bytes-copy! bstr dest-start src)]
    [(bstr dest-start src src-start) (bytes-copy! bstr dest-start src src-start)]
    [(bstr dest-start src src-start src-end) (bytes-copy! bstr dest-start src src-start src-end)]))

(define-sequence-syntax Bytes.to_sequence/optimize
  (lambda () #'Bytes.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ bstr-expr)]
       #`[(id) (in-bytes #,(discard-static-infos #'bstr-expr))]]
      [_ #f])))

(define/method (Bytes.to_sequence bstr)
  #:primitive (in-bytes)
  #:static-infos ((#%call-result ((#%sequence-constructor #t))))
  (in-bytes bstr))

(define/method (Bytes.snapshot bstr)
  #:primitive (bytes->immutable-bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (bytes->immutable-bytes bstr))

(define (bytes!=? a b)
  (if (and (bytes? a) (bytes? b))
      (not (bytes=? a b))
      (raise-argument-error* '!= rhombus-realm "Bytes" (if (bytes? a) b a))))

(define (bytes<=? a b)
  (if (and (bytes? a) (bytes? b))
      (not (bytes>? a b))
      (raise-argument-error* '<= rhombus-realm "Bytes" (if (bytes? a) b a))))

(define (bytes>=? a b)
  (if (and (bytes? a) (bytes? b))
      (not (bytes<? a b))
      (raise-argument-error* '>= rhombus-realm "Bytes" (if (bytes? a) b a))))

(begin-for-syntax
  (install-get-literal-static-infos! 'bytes get-bytes-static-infos))
