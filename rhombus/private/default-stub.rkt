#lang racket/base
(require (for-syntax racket/base))

;; export identifiers that otherwise have no default-space binding from `rhombus`

(define-syntax-rule (out id ...)
  (begin
    (define-syntax id #f)
    ...
    (provide id ...)))

(out as
     open
     expose
     rename
     only
     except
     only_space
     except_space
     meta
     meta_label
     names
     all_from
     NonemptyList

     pattern
     description
     kind
     error_mode
     fields

     matching_also
     matching_when
     matching_unless)
