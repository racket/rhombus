#lang racket/base

(provide syntax-raw-property
         syntax-raw-prefix-property
         syntax-raw-inner-prefix-property
         syntax-raw-inner-suffix-property
         syntax-raw-suffix-property
         syntax-raw-tail-property
         syntax-opaque-raw-property
         syntax-raw-opaque-content-property
         syntax-raw-srcloc-property)

(define syntax-raw-property
  (case-lambda
    [(stx) (syntax-property stx 'raw)]
    [(stx val) (syntax-property stx 'raw val #t)]))

(define syntax-raw-prefix-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-prefix)]
    [(stx val) (syntax-property stx 'raw-prefix val #t)]))

;; Just after a prefix, but sticks to a term instead of an
;; enclosing group, and counts as part of an enclosing group's
;; non-prefix content
(define syntax-raw-inner-prefix-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-inner-prefix)]
    [(stx val) (syntax-property stx 'raw-inner-prefix val #t)]))

;; Just before a prefix, sticks to a term, etc.
(define syntax-raw-inner-suffix-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-inner-suffix)]
    [(stx val) (syntax-property stx 'raw-inner-suffix val #t)]))

;; When attached to the head of a container encodings, applies
;; after the "tail"
(define syntax-raw-suffix-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-suffix)]
    [(stx val) (syntax-property stx 'raw-suffix val #t)]))

;; "tail" is attached to the head term of a list, and it
;; applies after the last item in the list, but counts as
;; the representation of the list itself
(define syntax-raw-tail-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-tail)]
    [(stx val) (syntax-property stx 'raw-tail val #t)]))

;; Hides any nested syntax and ignores an immediate 'raw property when
;; present and not #f; this is an emphemeral property and is attached
;; to the "parentheses" of a group or compound term
(define syntax-opaque-raw-property
  (case-lambda
    [(stx) (syntax-property stx 'opaque-raw)]
    [(stx val) (syntax-property stx 'opaque-raw val)]))

;; Similar to `syntax-opaque-raw-property`, but attached to the
;; head element of a compound value, and is not ephemeral
(define syntax-raw-opaque-content-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-opaque-content)]
    [(stx val) (syntax-property stx 'raw-opaque-content val #t)]))

;; For associating a srcloc to a compound tag like `group`, which
;; otherwise gets a derived srcloc based on its content
(define syntax-raw-srcloc-property
  (case-lambda
    [(stx) (syntax-property stx 'raw-srcloc)]
    [(stx val) (syntax-property stx 'raw-srcloc val #t)]))
