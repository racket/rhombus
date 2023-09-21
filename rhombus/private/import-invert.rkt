#lang racket/base
(require syntax/parse/pre
         "space-in.rkt")

(provide import-invert)

(define (import-invert r orig-stx orig-r)
  (let loop ([r r] [ctx #f])
    (syntax-parse r
      #:datum-literals (rename-in only-in except-in expose-in for-label for-meta only-meta-in
                                  rhombus-prefix-in only-spaces-in except-spaces-in)
      [((~and tag rhombus-prefix-in) imp name . rest)
       (define-values (mp new-imp) (loop #'imp (or (and (identifier? #'name)
                                                        #'name)
                                                   ctx)))
       (values mp #`(tag #,new-imp name . rest))]
      [((~and tag (~or rename-in only-in except-in expose-in only-spaces-in except-spaces-in)) imp . rest)
       (define-values (mp new-imp) (loop #'imp ctx))
       (values mp #`(tag #,new-imp . rest))]
      [((~and tag for-label) imp)
       (define-values (mp new-imp) (loop #'imp ctx))
       (values mp #`(tag #,new-imp))]
      [((~and tag (~or for-meta only-meta-in)) phase imp)
       (define-values (mp new-imp) (loop #'imp ctx))
       (values mp #`(tag phase #,new-imp))]
      [_
       (values (if ctx
                   ;; make context of import match a prefix id:
                   (datum->syntax ctx (syntax-e r) r r)
                   r)
               #f)])))
