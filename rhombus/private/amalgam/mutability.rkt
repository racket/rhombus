#lang racket/base
(require "../version-case.rkt")

(provide immutable-string?
         mutable-string?
         immutable-bytes?
         mutable-bytes?
         immutable-vector?
         mutable-vector?
         immutable-hash?
         mutable-hash?
         immutable-box?
         mutable-box?)

;; TEMP approximate `racket/mutability`
(meta-if-version-at-least
 "8.9.0.3"
 (require racket/mutability)
 (begin
   (define (immutable-string? v) (and (string? v) (immutable? v)))
   (define (mutable-string? v) (and (string? v) (not (immutable? v))))

   (define (immutable-bytes? v) (and (bytes? v) (immutable? v)))
   (define (mutable-bytes? v) (and (bytes? v) (not (immutable? v))))

   (define (immutable-vector? v) (and (vector? v) (immutable? v)))
   (define (mutable-vector? v) (and (vector? v) (not (immutable? v))))

   (define (immutable-hash? v) (and (hash? v) (immutable? v)))
   (define (mutable-hash? v) (and (hash? v) (not (immutable? v))))

   (define (immutable-box? v) (and (box? v) (immutable? v)))
   (define (mutable-box? v) (and (box? v) (not (immutable? v))))))
