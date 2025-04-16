#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "key-comp-runtime.rkt"
         "annotation-failure.rkt"
         "name-equal.rkt")

;; Define the implementation part of `equal_name_and_scopes` so we
;; can use it in the annotation-macro protocol

(provide equal-name-and-scopes-map?
         wrap-equal-name-and-scopes-map
         empty-equal_name_and_scopes-map)

(define-syntax (define-bound-id-map stx)
  (syntax-parse stx
    [(_ id id? wrap-id)
     #`(begin
         #,@(build-key-comp-runtime #'id #'bound-id=? #'hash-bound-id= #'id? #'wrap-id))]))

(define (bound-id=? a b recur)
  (equal-name-and-scopes? 'equal_name_and_scopes a b (syntax-local-phase-level)))

(define (hash-bound-id= a recur)
  (recur (name-to-symbol 'equal_name_and_scopes a)))

(define-bound-id-map equal_name_and_scopes
  equal-name-and-scopes-map?
  wrap-equal-name-and-scopes-map)

(define empty-equal_name_and_scopes-map
  (wrap-equal-name-and-scopes-map (hash)))
