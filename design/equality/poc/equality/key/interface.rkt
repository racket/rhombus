#lang racket/base

(provide gen:comparable
         comparable?
         key
         full-key
         type-tag)

(require racket/generic
         racket/contract/base
         (only-in racket/function identity))

(define-values (prop:type-tag type-tag? type-tag)
  (make-struct-type-property 'type-tag))

(define-generics comparable
  (key comparable)
  (full-key comparable)
  ;; For custom types, full-key has a default
  ;; implementation that affixes the type tag onto the
  ;; chosen representative. It should not typically be
  ;; overridden by users, unless they want to override
  ;; the default behavior that treats types as disjoint
  ;; from the perspective of equality.
  #:defaults
  ([struct? ; custom types
    ;; For custom types, by default, the key is a
    ;; vector-of-field-values.  Additionally, in a real
    ;; implementation, full-key must affix a unique type
    ;; tag to this key.  It does not appear that we can
    ;; accomplish this exclusively at the generic
    ;; interfaces level using defaults and fallbacks,
    ;; since a custom type that does not explicitly
    ;; declare gen:comparable does not inherit the
    ;; type-tag? struct type property.
    (define key struct->vector)
    (define full-key struct->vector)]
   [any/c ; built-in (key) types
    ;; for an instance of a key type, the key is itself
    (define key identity)
    (define full-key identity)])
  #:fallbacks
  [(define key identity)
   (define (full-key this)
     (cons (type-tag this) (key this)))]
  #:derive-property prop:type-tag (gensym))
