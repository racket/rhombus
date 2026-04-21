#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/provide-transform
                     racket/phase+space
                     "skip-only-except-key.rkt"))

(provide only-meta-out
         except-meta-out)

(define-for-syntax (make-transformer prune)
  (make-provide-transformer
   (lambda (stx phase+spaces)
     (syntax-parse stx
       [(_ prov phase ...)
        (define phases (for/hasheqv ([ph (in-list (syntax->list #'(phase ...)))])
                         (values (syntax-e ph) #t)))
        (define exports (expand-export #'prov phase+spaces))
        (for/list ([ex (in-list exports)]
                   #:when (let ([phase (or (syntax-property (export-orig-stx ex) skip-only/except-space-key)
                                           (phase+space-phase (export-mode ex)))])
                            (if (eq? prune 'only)
                                (hash-ref phases phase #f)
                                (not (hash-ref phases phase #f)))))
          ex)]))))

(define-syntax only-meta-out
  (make-transformer 'only))

(define-syntax except-meta-out
  (make-transformer 'except))
