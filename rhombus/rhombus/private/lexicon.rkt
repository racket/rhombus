#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/lexicon
                     "srcloc.rkt")
         "dot.rkt")

(provide (for-syntax simple-lexicon))

(begin-for-syntax
  (define-syntax-rule (simple-lexicon id ...)
    (make-simple-lexicon (make-hasheq (list (cons 'id (quote-syntax id)) ...))))
  
  (define (make-simple-lexicon ht)
    (lexicon
     (lambda (stxes)
       (syntax-parse stxes
         #:datum-literals (op)
         #:literals (|.|)
         [(form-id (op |.|) key:identifier . tail)
          (define id (hash-ref ht (syntax-e #'key) #f))
          (unless id
            (raise-syntax-error #f
                                (format "not provided by ~a"
                                        (syntax-e #'form-id))
                                #'key))
          (values (relocate #'key id) #'tail)])))))

