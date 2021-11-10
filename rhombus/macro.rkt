#lang racket/base
(require "private/bounce.rkt")

(require (for-syntax "main.rkt"))
(provide (for-syntax (all-from-out "main.rkt")))

(bounce "private/expression-syntax.rkt"
        "private/binding-syntax.rkt"
        "private/definition-syntax.rkt"
        "private/declaration-syntax.rkt"
        "private/annotation-syntax.rkt"
        "private/static-info-syntax.rkt"
        "private/dot-syntax.rkt"
        "private/syntax-error.rkt"
        "private/parsed.rkt")
