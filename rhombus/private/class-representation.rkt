#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (submod "annotation.rkt" for-class))

(provide (for-syntax get-representation-annotation))

(define-for-syntax (get-representation-annotation as-ann-seq orig-stx)
  (cond
    [(not (syntax-e as-ann-seq)) (values #f #f)]
    [else
     (syntax-parse #`(group . #,as-ann-seq)
       [ann::annotation
        (syntax-parse #'ann.parsed
          [ann-parsed::annotation-predicate-form
           (values #'ann-parsed.predicate
                   #'ann-parsed.static-infos)]
          [_
           (raise-syntax-error #f
                               "converter annotation not supported for class representation"
                               orig-stx
                               as-ann-seq)])])]))
