#lang racket/base
(require "private/bounce.rkt")

(require (for-syntax "main.rkt"))
(provide (for-syntax (all-from-out "main.rkt")))

(bounce "private/meta.rkt"
        "private/expression-syntax.rkt"
        "private/binding-syntax.rkt"
        "private/definition-syntax.rkt"
        "private/declaration-syntax.rkt"
        "private/annotation-syntax.rkt"
        "private/reducer-syntax.rkt"
        "private/for-clause-syntax.rkt"
        "private/class-clause-syntax.rkt"
        (submod "private/class.rkt" for-meta)
        "private/callable-syntax.rkt"
        "private/static-info-syntax.rkt"
        "private/dot-syntax.rkt"
        "private/import-syntax.rkt"
        "private/export-syntax.rkt"
        "private/syntax-error.rkt"
        "private/parsed.rkt"
        "private/syntax-meta-value.rkt")
