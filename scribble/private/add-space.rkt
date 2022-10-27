#lang racket/base

(provide add-space)

(define (add-space stx space-name)
  (cond
    [(eq? space-name 'hide)
     (quote-syntax never-bound)]
    [else
     (define space (case space-name
                     [(bind) 'rhombus/binding]
                     [(impmod) 'rhombus/import]
                     [(expmod) 'rhombus/export]
                     [(annot) 'rhombus/annotation]
                     [(stxclass) 'rhombus/syntax-class]
                     [(reducer) 'rhombus/reducer]
                     [(for_clause) 'rhombus/for-clause]
                     [(class_clause) 'rhombus/class-clause]
                     [(entry_point) 'rhombus/entry-point]
                     [(#f) 'rhombus/expression]
                     [else #f]))
     (if space
         ((make-interned-syntax-introducer space) stx 'add)
         stx)]))
