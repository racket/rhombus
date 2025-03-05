#lang racket/base
(require (submod "annotation.rkt" for-class))

(provide wrap-progress-evt
         extract-progress-evt
         extract-commit-evt
         set-evt!)

(define do-wrap-progress-evt (lambda (x) x))
(define do-extract-progress-evt (lambda (x) (and (progress-evt? x) x)))
(define do-extract-commit-evt (lambda (x) (and (evt? x) x)))

(define (set-evt! wrap-proc extract-proc extract-commit-proc)
  (set! do-wrap-progress-evt wrap-proc)
  (set! do-extract-progress-evt extract-proc)
  (set! do-extract-commit-evt extract-commit-proc))

(define (wrap-progress-evt v)
  (do-wrap-progress-evt v))

(define (extract-progress-evt who evt)
  (define p-evt (do-extract-progress-evt evt))
  (when who
    (unless (and p-evt (progress-evt? p-evt))
      (raise-annotation-failure who evt "ProgressEvt")))
  p-evt)

(define (extract-commit-evt who evt)
  (define c-evt (do-extract-commit-evt evt))
  (when (and who (not c-evt))
    (raise-annotation-failure who evt "CommitEvt"))
  c-evt)
