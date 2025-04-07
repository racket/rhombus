#lang racket/base

(provide make-deprecated-rename-transformer
         warn-deprecated!)

(module+ struct
  (provide (struct-out deprecated-rename-transformer)))

(struct deprecated-rename-transformer (to-id name date)
  #:property prop:rename-transformer (struct-field-index to-id))

(define (make-deprecated-rename-transformer to-id name date)
  (define who 'make-deprecated-rename-transformer)
  (unless (identifier? to-id) (raise-argument-error who "identifier?" to-id))
  (unless (symbol? name) (raise-argument-error who "symbol?" name))
  (unless (string? date) (raise-argument-error who "string?" date))
  (deprecated-rename-transformer (syntax-property to-id 'not-free-identifier=? #t) name date))

(define already-complained (make-hasheq))

(define (warn-deprecated! name date-str)
  (unless (hash-ref already-complained name #f)
    (hash-set! already-complained name #t)
    (log-error (string-append-immutable
                "warning (logged as as error): reference to a deprecated name\n"
                "  name: ~a\n"
                "  to be removed after: ~a")
               name
               date-str)))
