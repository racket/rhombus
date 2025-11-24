#lang racket/base

(provide lookup-space-description)

(define (lookup-space-description space-sym)
  (case space-sym
    [(#f) "expressions, definitions, and declarations"]
    [(rhombus/repet) "repetitions"]
    [(rhombus/annot) "annotations"]
    [(rhombus/bind) "bindings"]
    [(rhombus/stxclass) "syntax classes"]
    [(rhombus/unquote_binding) "unquote binding"]
    [(rhombus/namespace) "namespaces"]
    [(rhombus/class) "classes and interfaces"]
    [(rhombus/for_clause) "for clauses"]
    [(rhombus/class_clause) "class clauses"]
    [(rhombus/interface_clause) "interface clauses"]
    [else #f]))
