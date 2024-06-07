#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/provide-transform
                     racket/phase+space))

(provide only-spaces-out
         except-spaces-out)

(define-for-syntax (make-transformer prune)
  (make-provide-transformer
   (lambda (stx phase+spaces)
     (syntax-parse stx
       [(_ prov space ...)
        (define spaces (for/hasheq ([sp (in-list (syntax->list #'(space ...)))])
                         (values (syntax-e sp) #t)))
        (define exports (expand-export #'prov phase+spaces))
        (for/list ([ex (in-list exports)]
                   #:when (if (eq? prune 'only)
                              (hash-ref spaces (phase+space-space (export-mode ex)) #f)
                              (not (hash-ref spaces (phase+space-space (export-mode ex)) #f))))
          ex)]))))

(define-syntax only-spaces-out
  (make-transformer 'only))

(define-syntax except-spaces-out
  (make-transformer 'except))
