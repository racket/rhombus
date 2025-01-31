#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "static-info.rkt")

;; value for #%compare is either
;;   #:method
;;   compare_to_id
;;   ((op id_or_box) ...)
;; where id_or_box is
;;   op_id
;;   &compare_to_id

(define-static-info-key-syntax/provide #%compare
  (static-info-key (lambda (a b)
                     (cond
                       [(and (eq? '#:method (syntax-e a)) (eq? '#:method (syntax-e b)))
                        a]
                       [(or (identifier? a) (identifier? b))
                        (static-info-identifier-or a b)]
                       [else
                        (let ([as (syntax->list a)]
                              [bs (syntax->list b)])
                          (and as
                               bs
                               ;; we could try to intersect at a finer granularity, but
                               ;; just check whether they're the same
                               (= (length as) (length bs))
                               (for/and ([a (in-list as)]
                                         [b (in-list bs)])
                                 (syntax-parse (list a b)
                                   [((a-op a-id:identifier) (b-op b-id:identifier))
                                    (and (eq? (syntax-e #'a-op) (syntax-e #'b-op))
                                         (free-identifier=? #'a-id #'b-id))]
                                   [((a-op #&a-id:identifier) (b-op #&b-id:identifier))
                                    (and (eq? (syntax-e #'a-op) (syntax-e #'b-op))
                                         (free-identifier=? #'a-id #'b-id))]
                                   [_ #false]))
                               a))]))
                   (lambda (a b)
                     a)))
