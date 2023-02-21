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
         "define-arity.rkt"
         "name-root.rkt"
         "static-info.rkt"
         "dot-parse.rkt"
         (submod "dot.rkt" for-dot-provider))

(provide (for-spaces (#f
                      rhombus/repet)

                     +&)
         (for-spaces (#f
                      rhombus/statinfo)
                     to_string)
         (for-spaces (rhombus/namespace)
                     String))

(module+ for-builtin
  (provide string-method-table))

(module+ static-infos
  (provide (for-syntax string-static-infos)))

(define-for-syntax string-static-infos
  #'((#%dot-provider string-instance)))

(define-infix +& append-as-strings
  #:stronger-than (== ===))

(define (append-as-strings a b)
  (string-append-immutable (to_string a)
                           (to_string b)))

(define/arity (to_string a)
  #:static-infos ((#%call-result #,string-static-infos))
  (cond
    [(string? a) a]
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
   to_number))

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

(define string-method-table
  (hash 'length (method1 string-length)
        'to_int (method1 to_int)
        'to_number (method1 to_number)))

(define-syntax string-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'string-length)]
        [(to_int) (0ary #'to_int)]
        [(to_number) (0ary #'to_number)]
        [else (fail-k)])))))

(begin-for-syntax
  (install-static-infos! 'string string-static-infos))
