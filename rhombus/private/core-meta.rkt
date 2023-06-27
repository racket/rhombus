#lang racket/base
(require "bounce.rkt")

(require (for-syntax "core.rkt"))
(provide (for-syntax (all-from-out "core.rkt")))

(bounce "meta.rkt"
        "expr-macro.rkt"
        "bind-macro.rkt"
        "defn-macro.rkt"
        "decl-macro.rkt"
        "annot-macro.rkt"
        "reducer-macro.rkt"
        "for-clause-macro.rkt"
        "class-clause-macro.rkt"
        "interface-clause-macro.rkt"
        "class-clause-primitive-meta-macro.rkt"
        "entry-point-macro.rkt"
        "static-info-macro.rkt"
        "repet-macro.rkt"
        "dot-macro.rkt"
        "assign-macro.rkt"
        "import-macro.rkt"
        "export-macro.rkt"
        "unquote-binding-macro.rkt"
        "syntax-class-clause-macro.rkt"
        "pattern-clause-macro.rkt"
        "syntax-meta.rkt"
        "space-macro.rkt"
        "space-clause-primitive.rkt"
        "namespace-macro.rkt")

(bounce-meta "space-meta-clause-primitive.rkt"
             "unquote-binding-primitive-meta.rkt")

