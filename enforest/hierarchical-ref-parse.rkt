#lang racket/base
(require syntax/parse
         "ref-parse.rkt"
         "syntax-local.rkt"
         "lexicon.rkt"
         (submod "lexicon.rkt" for-parse))

(provide :hierarchical-ref-seq)

(define-syntax-class (:hierarchical-ref-seq in-space hierarchy-op)
  #:datum-literals (op)
  (pattern (~and stxes (ref::reference (op sep) . _))
           #:when (eq? hierarchy-op (syntax-e #'sep))
           #:do [(define head-id (in-space #'ref.name))
                 (define lxc (syntax-local-value* head-id lexicon-ref))]
           #:when lxc
           #:do [(define-values (head tail) (apply-lexicon head-id lxc #'stxes))]
           #:with (~var href (:hierarchical-ref-seq in-space hierarchy-op)) (cons head tail)
           #:attr name #'href.name
           #:attr tail #'href.tail)
  (pattern (ref::reference . tail)
           #:attr name #'ref.name))
