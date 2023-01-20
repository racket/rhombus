#lang racket/base
(require syntax/parse/pre
         "name-parse.rkt"
         "syntax-local.rkt"
         (submod "name-root.rkt" for-parse))

(provide :hier-name-seq)

(define-syntax-class (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)
  #:datum-literals (op)
  #:description "identifier or dotted identifier"
  (pattern (~and stxes (root::name (op sep) . _))
           #:when (eq? name-path-op (syntax-e #'sep))
           #:do [(define head-id (in-name-root-space #'root.name))
                 (define lxc (syntax-local-value* head-id name-root-ref))]
           #:when lxc
           #:cut
           #:do [(define-values (head tail) (apply-name-root head-id lxc in-space #'stxes))]
           #:with (~var hname (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)) (cons head tail)
           #:attr name #'hname.name
           #:attr tail #'hname.tail)
  (pattern (base::name . tail)
           #:attr name #'base.name))
