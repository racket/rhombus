#lang racket/base
(require "private/bounce.rkt")

(require (for-syntax "main.rkt"))
(provide (for-syntax (all-from-out "main.rkt")))

(bounce "private/for-meta.rkt"
        "private/expression-syntax.rkt"
        "private/binding-syntax.rkt"
        "private/definition-syntax.rkt"
        "private/declaration-syntax.rkt"
        "private/annotation-syntax.rkt"
        "private/folder-syntax.rkt"
        "private/static-info-syntax.rkt"
        "private/dot-syntax.rkt"
        "private/import-syntax.rkt"
        "private/syntax-error.rkt"
        "private/parsed.rkt"
        "private/syntax-meta-value.rkt"
        (submod "private/syntax-class-syntax.rkt" for-macro))

(require (only-in "private/import.rkt" for_meta))
(provide (for-space rhombus/import for_meta))
