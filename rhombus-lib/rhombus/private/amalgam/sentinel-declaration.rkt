#lang racket/base
(require (for-syntax racket/base)
         "declaration.rkt"
         "nestable-declaration.rkt")

(provide (for-space rhombus/decl
                    sentinel_declaration))

;; Used in the expansion of a `class` body (and similar),
;; where the expansion of a clause should not be treated
;; as a sequence of tail expressions and definitions
(define-decl-syntax sentinel_declaration
  (nestable-declaration-transformer
   (lambda (stx name-prefix)
     #'())))
