#lang racket

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
     meta
     meta_label
     names
     all_from)
