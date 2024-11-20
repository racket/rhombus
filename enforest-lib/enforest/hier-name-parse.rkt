#lang racket/base
(require syntax/parse/pre
         "name-parse.rkt"
         "syntax-local.rkt"
         (submod "name-root.rkt" for-parse))

(provide :hier-name-seq)

(define-syntax-class (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)
  #:attributes (name head tail)
  #:datum-literals (op)
  #:description "name or dotted name"
  (pattern (~and stxes (root::name (~and o (op sep)) . orig-tail))
           #:when (eq? name-path-op (syntax-e #'sep))
           #:do [(define head-id (in-name-root-space #'root.name))
                 (define lxc (syntax-local-value* head-id name-root-ref))]
           #:when lxc
           #:cut
           #:do [(define-values (head tail) (apply-name-root head-id lxc in-space #'stxes))]
           #:with (~var r (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)) #`(#,head . #,tail)
           #:with (next-head . _) #'orig-tail
           #:attr name #'r.name
           #:attr head #`(root o next-head . #,(cdr (syntax-e #'r.head)))
           #:attr tail #'r.tail)
  (pattern (n::name . tail)
           #:attr head #'(n)
           #:attr name #'n.name))
