#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/name-root
                     "srcloc.rkt")
         "dot.rkt")

(provide (for-syntax simple-name-root))

(begin-for-syntax
  (define-syntax-rule (simple-name-root id ...)
    (make-simple-name-root (make-hasheq (list (cons 'id (quote-syntax id)) ...))))
  
  (define (make-simple-name-root ht)
    (name-root
     (lambda (stxes)
       (syntax-parse stxes
         #:datum-literals (op |.|)
         [(form-id (op |.|) key:identifier . tail)
          (define id (hash-ref ht (syntax-e #'key) #f))
          (unless id
            (raise-syntax-error #f
                                (format "not provided by ~a"
                                        (syntax-e #'form-id))
                                #'key))
          (values (relocate #'key id) #'tail)]
         [(form-id (op (~and dot |.|)) . tail)
          (raise-syntax-error #f
                              "expected an identifier after dot"
                              #'dot)]
         [(form-id . tail)
          (raise-syntax-error #f
                              "expected a dot and then an identifier"
                              #'form-id)])))))
