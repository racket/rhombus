#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         "parse.rkt"
         "expression.rkt"
         "binding.rkt")

(provide cons
         (for-space rhombus/binding cons))

(define-binding-syntax cons
   (binding-transformer
    (lambda (tail)
      (syntax-parse tail
        [(_ ((~datum parens) a::binding d::binding) . new-tail)
         #:with a-expanded::binding-form #'a.expanded
         #:with d-expanded::binding-form #'d.expanded
         (with-syntax ([falses (for/list ([b (in-range (+ (length (syntax->list #'a-expanded.var-ids))
                                                          (length (syntax->list #'d-expanded.var-ids))))])
                                 #'#f)])
           (values
            #'([a-expanded.var-id ... d-expanded.var-id ...]
               (lambda (v)
                 (if (pair? v)
                     (let-values ([(a-match? a-expanded.var-id ...) (a-expanded.check-proc-expr (car v))])
                       (if a-match?
                           (let-values ([(d-match? d-expanded.var-id ...) (d-expanded.check-proc-expr (cdr v))])
                             (values d-match? a-expanded.var-id ... d-expanded.var-id ...))
                           (values #f . falses)))
                     (values #f . falses)))
               (begin
                 a-expanded.post-defn
                 d-expanded.post-defn))
            #'new-tail))]))))
