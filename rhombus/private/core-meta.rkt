#lang racket/base
(require "bounce.rkt")

(require (for-syntax "core.rkt"))
(provide (for-syntax (all-from-out "core.rkt")))

(bounce "meta.rkt"
        "expression-syntax.rkt"
        "binding-syntax.rkt"
        "definition-syntax.rkt"
        "declaration-syntax.rkt"
        "annotation-syntax.rkt"
        "reducer-syntax.rkt"
        "for-clause-syntax.rkt"
        "class-clause-syntax.rkt"
        "interface-clause-syntax.rkt"
        "entry-point-syntax.rkt"
        "static-info-syntax.rkt"
        "repetition-syntax.rkt"
        "dot-syntax.rkt"
        "import-syntax.rkt"
        "export-syntax.rkt"
        "syntax-error.rkt"
        "parsed.rkt"
        "syntax-meta-value.rkt"
        "sublanguage-syntax.rkt")
