#lang racket/base
(require syntax/parse
         "ref-parse.rkt"
         "syntax-local.rkt"
         "name-root.rkt"
         (submod "name-root.rkt" for-parse))

(provide :name-ref-seq)

(define-syntax-class (:name-ref-seq in-space name-path-op)
  #:datum-literals (op)
  (pattern (~and stxes (ref::reference (op sep) . _))
           #:when (eq? name-path-op (syntax-e #'sep))
           #:do [(define head-id (in-space #'ref.name))
                 (define lxc (syntax-local-value* head-id name-root-ref))]
           #:when lxc
           #:do [(define-values (head tail) (apply-name-root head-id lxc #'stxes))]
           #:with (~var href (:name-ref-seq in-space name-path-op)) (cons head tail)
           #:attr name #'href.name
           #:attr tail #'href.tail)
  (pattern (ref::reference . tail)
           #:attr name #'ref.name))
