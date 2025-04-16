#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "key-comp-property.rkt"
         "../version-case.rkt")

(meta-if-version-at-least
 "8.13.0.2"
 (require (only-in '#%unsafe unsafe-impersonate-hash))
 (define (unsafe-impersonate-hash x ht . _)
   ;; does not work right, but lets things compile
   ht))

(provide (for-syntax build-key-comp-runtime))

(define-for-syntax (build-key-comp-runtime name-id x-equals?-id x-hash-code-id x-map?-id wrap-id)
  (with-syntax ([name name-id]
                [x-equals? x-equals?-id]
                [x-hash-code x-hash-code-id]
                [x-map? x-map?-id]
                [wrap wrap-id])
    #`(;; keys are wrapped in this struct, which lets use our own
       ;; hash function for the keys
       (struct x (v)
         #:authentic
         #:sealed
         #:property prop:equal+hash (list (lambda (a b recur mode)
                                            (x-equals? (x-v a) (x-v b) recur))
                                          (lambda (a recur mode)
                                            (x-hash-code (x-v a) recur))))
       (define x-custom-map (custom-map 'name
                                        (lambda ()
                                          (wrap #hash()))
                                        (lambda ()
                                          (wrap (make-hash)))
                                        (lambda ()
                                          (wrap (make-ephemeron-hash)))))
       (define (x-map? v) (eq? (custom-map-ref v #f) x-custom-map))
       (define (wrap ht)
         (unsafe-impersonate-hash x-custom-map ;; kind for `equal?`
                                  ht
                                  ;; ref
                                  (lambda (ht key)
                                    (values (x key)
                                            (lambda (ht key val) val)))
                                  ;; set
                                  (if (not (hash-ephemeron? ht))
                                      (lambda (ht key val)
                                        (values (x key) val))
                                      ;; need to establish a connection between each
                                      ;; key and its wrapped key:
                                      (let ([keys (make-ephemeron-hasheq)])
                                        (lambda (ht key val)
                                          (define x-key (x key))
                                          (hash-set! keys x-key key)
                                          (values x-key val))))
                                  ;; remove
                                  (lambda (ht key)
                                    (x key))
                                  ;; key
                                  (lambda (ht key)
                                    (x-v key))
                                  ;; clear
                                  (lambda (ht) (void))
                                  prop:custom-map x-custom-map)))))

