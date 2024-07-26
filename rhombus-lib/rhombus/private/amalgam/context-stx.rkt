#lang racket/base
(require syntax/parse/pre
         "pack.rkt"
         "realm.rkt")

(provide extract-ctx
         extract-group-ctx)

(define (extract-ctx who ctx-stx
                     #:false-ok? [false-ok? #t]
                     #:update [update #f])
  (and (or (not false-ok?) ctx-stx)
       (let ([t (and (syntax? ctx-stx)
                     (unpack-term ctx-stx #f #f))])
         (unless t
           (raise-argument-error* who rhombus-realm (if false-ok? "maybe(Term)" "Term") ctx-stx))
         (syntax-parse t
           #:datum-literals (op)
           [((~and tag op) id) (if update
                                   (datum->syntax t (list #'tag (update #'id)) t t)
                                   #'id)]
           [(head . tail) (if update
                              (datum->syntax t (cons (update #'head) #'tail) t t)
                              #'head)]
           [_ (if update
                  (update t)
                  t)]))))

(define (extract-group-ctx who ctx-stx
                           #:false-ok? [false-ok? #t]
                           #:update [update #f])
  (and (or (not false-ok?) ctx-stx)
       (let ([t (and (syntax? ctx-stx)
                     (unpack-group ctx-stx #f #f))])
         (unless t
           (raise-argument-error* who rhombus-realm (if false-ok? "maybe(Group)" "Group") ctx-stx))
         (syntax-parse t
           #:datum-literals (group)
           [((~and tag group) . tail)
            (if update
                (datum->syntax t (cons (update #'tag) #'tail) t t)
                #'tag)]
           [_ (if update
                  (update t)
                  t)]))))
