#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     "name-path-op.rkt")
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "parens.rkt")

(provide (for-syntax do-ends-parse?))

(define-for-syntax (do-ends-parse? left-mode left-id tail
                                   in-space relative-precedence infix-operator-ref)
  (define (compare name)
    (define rel (relative-precedence left-mode left-id 'infix name))
    (eq? rel 'stronger))
  (syntax-parse tail
    [() #t]
    [(~var name (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref/maybe))
     (compare #'name.name)]
    [((tag::parens . _) . _) (compare (datum->syntax #'tag '#%call))]
    [((tag::brackets . _) . _) (compare (datum->syntax #'tag '#%index))]
    [((tag::braces . _) . _) (compare (datum->syntax #'tag '#%braces))]
    [((tag::quotes . _) . _) (compare (datum->syntax #'tag '#%quotes))]
    [_ #f]))
