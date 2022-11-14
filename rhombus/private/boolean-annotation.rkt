#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "annotation-string.rkt")
         "annotation.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "parse.rkt")

(provide (for-space rhombus/annotation
                    &&
                    \|\|))

(define-annotation-syntax &&
  (annotation-infix-operator
   #'&&
   (list (cons #'\|\| 'stronger))
   'macro
   (lambda (lhs stx)
     (syntax-parse stx
       [rhs::annotation-infix-op+form+tail
        #:with l::annotation-form lhs
        #:with r::annotation-form #'rhs.parsed
        #:with (r-static-info ...) #'r.static-infos
        (values #`((let ([l-pred l.predicate]
                         [r-pred r.predicate])
                     (lambda (v)
                       (and (l-pred v) (r-pred v))))
                   (r-static-info ... . l.static-infos))
                #'rhs.tail)]))
   'left))

(define-annotation-syntax \|\|
  (annotation-infix-operator
   #'\|\|
   null
   'macro
   (lambda (lhs stx)
     (syntax-parse stx
       [rhs::annotation-infix-op+form+tail
        #:with l::annotation-form lhs
        #:with (l-static-info ...) #'l.static-infos
        #:with r::annotation-form #'rhs.parsed
        (values #`((let ([l-pred l.predicate]
                         [r-pred r.predicate])
                     (lambda (v)
                       (or (l-pred v) (r-pred v))))
                   #,(static-infos-intersect #'l.static-infos #'r.static-infos))
                #'rhs.tail)]))
   'left))
