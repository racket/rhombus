#lang racket/base

(provide convert-empty-group
         convert-empty-alts
         error-empty-group)

(define (convert-empty-group at-depth l)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax-e l)))
     (if (or (null? u)
             (and (syntax? u) (null? (syntax-e u))))
         null
         (list l))]
    [else (for/list ([g (in-list (syntax->list l))])
            (convert-empty-group (sub1 at-depth) g))]))

(define (convert-empty-alts at-depth l)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax-e l)))
     (cond
       [(or (null? u)
            (and (syntax? u) (null? (syntax-e u))))
        (define a (car (syntax-e l)))
        (list (datum->syntax a 'block a a))]
       [else l])]
    [else (for/list ([g (in-list (syntax->list l))])
            (convert-empty-alts (sub1 at-depth) g))]))

(define (error-empty-group at-depth l)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax-e l)))
     (when (or (null? u)
               (and (syntax? u) (null? (syntax-e u))))
       (error '? "generated an empty group"))
     l]
    [else (for/list ([g (in-list (syntax->list l))])
            (error-empty-group (sub1 at-depth) g))]))
