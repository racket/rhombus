#lang racket/base
(require racket/mutability
         "annotation-failure.rkt")

(provide immutable-path-string?
         check-path-string
         check-cross-path-string)

(define (immutable-path-string? p)
  (and (path-string? p)
       (or (not (string? p))
           (immutable-string? p))))

(define (check-path-string who p)
  (unless (immutable-path-string? p)
    (raise-annotation-failure who p "PathString")))

(define (check-cross-path-string who p)
  (unless (or (immutable-path-string? p)
              (path-for-some-system? p))
    (raise-annotation-failure who p "PathString || CrossPath")))
