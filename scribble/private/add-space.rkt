#lang racket/base

(provide add-space)

(define (add-space stx space-name)
  (define space (case space-name
                  [(bind) 'rhombus/binding]
                  [(impmod) 'rhombus/import]
                  [(ann) 'rhombus/annotation]
                  [(stxclass) 'rhombus/syntax-class]
                  [(reducer) 'rhombus/reducer]
                  [(for_clause) 'rhombus/for-clause]
                  [else #f]))
  (if space
      ((make-interned-syntax-introducer space) stx 'add)
      stx))
