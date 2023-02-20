#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "name-path-op.rkt")
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "space.rkt")

(provide (for-syntax parse-space-names))

(define-for-syntax (parse-space-names stx gs)
  (apply
   append
   (for/list ([g (in-list (syntax->list gs))])
     (let loop ([g g])
       (syntax-parse g
         [() null]
         [(~var h (:hier-name-seq in-name-root-space in-space-space name-path-op name-root-ref))
          (define sp (syntax-local-value* (in-space-space #'h.name) space-name-ref))
          (unless (space-name? sp)
            (raise-syntax-error #f
                                "not bound as a space"
                                stx
                                #'h.name))
          (cons (space-name-symbol sp)
                (loop #'h.tail))])))))
