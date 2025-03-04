#lang racket/base
(require syntax/stx
         "pack.rkt")

(provide convert-empty-group
         convert-empty-alts
         check-misformed-group
         check-empty-or-misformed-group)

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

(define (convert-empty-alts at-depth l just-after-block?)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax-e l)))
     (cond
       [(or (null? u)
            (and (syntax? u) (null? (syntax-e u))))
        (define a (car (syntax-e l)))
        (if just-after-block?
            null
            (list (datum->syntax a 'block a a)))]
       [else (if just-after-block?
                 (list l)
                 l)])]
    [else (for/list ([g (in-list (syntax->list l))])
            (convert-empty-alts (sub1 at-depth) g))]))

(define (check-group at-depth l empty-ok?)
  (cond
    [(zero? at-depth)
     (define parts (syntax->list l))
     (define tag (car parts))
     (define u (syntax->list (cadr parts)))
     (define tail (caddr parts))
     (when (and (not empty-ok?) (null? u) (stx-null? tail))
       (error '|'| "generated an empty group"))
     (check-valid-group '|'| u tail)
     (datum->syntax #f (cons tag (if (stx-null? tail) u (append u tail))) #f #f)]
    [else (for/list ([g (in-list (syntax->list l))])
            (check-group (sub1 at-depth) g empty-ok?))]))
  
(define (check-empty-or-misformed-group at-depth l)
  (check-group at-depth l #f))

(define (check-misformed-group at-depth l)
  (check-group at-depth l #t))
