#lang racket/base

(provide add-binder
         add-binders)

;; TEMP These should be changed to use 'disappeared-binding when the
;; Check Syntax change is in place.

(define (add-binder id stx)
  (define p (syntax-property stx 'origin))
  (syntax-property stx 'origin (if p (cons id p) id)))

(define (add-binders ids stx)
  (cond
    [(null? ids) stx]
    [else
     (define p (syntax-property stx 'origin))
     (syntax-property stx 'origin (if p (cons ids p) ids))]))
