#lang racket/base

(provide compose-attr-name
         inline-attr-depth
         inline-form-id)

(define (compose-attr-name match-id sym id)
  ;; when `match-id` is #f, then we're dealing with inline `pattern`s
  (if match-id
      (datum->syntax match-id (string->symbol (format "~a.~a" (syntax-e match-id) sym)) id)
      (datum->syntax (or id (inline-form-id))
                     (string->symbol (format "~a.~a.~a" (syntax-e (inline-form-id)) (inline-attr-depth) sym))
                     id)))

;; handle nested inline `pattern`s
(define inline-attr-depth (make-parameter #f))

;; handle `open`ed syntax classes
(define inline-form-id (make-parameter #f))
