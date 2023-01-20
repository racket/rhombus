#lang racket/base
(require (for-syntax racket/base))

(provide identifier-binding*
         identifier-distinct-binding*)

(define (identifier-binding* id1 [phase (syntax-local-phase-level)])
  (identifier-binding id1 phase #t))

(define (identifier-distinct-binding* id1 id2 [phase (syntax-local-phase-level)])
  ;; Temporary accomodation for older versions of Racket
  (define-syntax (call stx)
    (cond
      [(string<? (version) "8.8.0.2")
       #'(identifier-distinct-binding id1 id2 phase)]
      [else
       #'(identifier-distinct-binding id1 id2 phase #t)]))
  (call))
