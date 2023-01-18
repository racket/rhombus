#lang racket/base

(provide add-space)

(define (add-space stx space-name)
  (cond
    [(eq? space-name 'hide)
     (quote-syntax never-bound)]
    [else
     (define space (case space-name
                     [(bind) 'rhombus/bind]
                     [(impo) 'rhombus/impo]
                     [(expo) 'rhombus/expo]
                     [(modpath) 'rhombus/modpath]
                     [(annot) 'rhombus/annot]
                     [(repet) 'rhombus/repet]
                     [(stxclass) 'rhombus/syntax/class]
                     [(reducer) 'rhombus/reducer]
                     [(for_clause) 'rhombus/for_clause]
                     [(class_clause) 'rhombus/class_clause]
                     [(intf_clause) 'rhombus/interface_clause]
                     [(entry_point) 'rhombus/entry_point]
                     [(syntax_binding) 'rhombus/unquote_bind]
                     [(syntax_class_clause) 'rhombus/syntax_class_clause]
                     [(pattern_clause) 'rhombus/pattern_clause]
                     [(#f) 'rhombus/expr]
                     [else #f]))
     (if space
         ((make-interned-syntax-introducer space) stx 'add)
         stx)]))
