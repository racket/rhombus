#lang racket/base
(require syntax/parse
         "name-parse.rkt"
         "syntax-local.rkt"
         (submod "name-root.rkt" for-parse))

(provide :hier-name-seq)

(define-syntax-class (:hier-name-seq in-space name-path-op name-root-ref)
  #:datum-literals (op)
  (pattern (~and stxes (root::name (op sep) . _))
           #:when (eq? name-path-op (syntax-e #'sep))
           #:do [(define head-id (in-space #'root.name))
                 (define lxc (syntax-local-value* head-id name-root-ref))]
           #:when lxc
           #:do [(define-values (head tail) (apply-name-root head-id lxc #'stxes))]
           #:with (~var hname (:hier-name-seq in-space name-path-op name-root-ref)) (cons head tail)
           #:attr name #'hname.name
           #:attr tail #'hname.tail)
  (pattern (base::name . tail)
           #:attr name #'base.name))
