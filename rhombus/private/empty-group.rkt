#lang racket/base
(require "pack.rkt")

(provide convert-empty-group
         convert-empty-alts
         error-misformed-group
         error-empty-or-misformed-group)

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

(define (error-empty-or-misformed-group at-depth l)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax->list l)))
     (when (null? u)
       (error '|'| "generated an empty group"))
     (unless (check-valid-group #f u)
       (error '|'| "generated a misformed group with a non-tail block or alternative"))
     l]
    [else (for/list ([g (in-list (syntax->list l))])
            (error-empty-or-misformed-group (sub1 at-depth) g))]))

(define (error-misformed-group at-depth l)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax->list l)))
     (unless (check-valid-group #f u)
       (error '|'| "generated a misformed group with a non-tail block or alternative"))
     l]
    [else (for/list ([g (in-list (syntax->list l))])
            (error-misformed-group (sub1 at-depth) g))]))
