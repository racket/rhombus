#lang racket/base
(require syntax/parse/pre)

(provide import-invert)

(define (import-invert r orig-stx orig-r)
  (let loop ([r r] [ctx #f])
    (syntax-parse r
      #:datum-literals (rename-in only-in except-in expose-in for-label for-meta only-meta-in
                                  rhombus-prefix-in only-spaces-in except-spaces-in
                                  immediate-namespace-in import-dotted)
      [((~and tag rhombus-prefix-in) imp name . rest)
       (define-values (mp new-imp lifted-nss) (loop #'imp (or (and (identifier? #'name)
                                                                   #'name)
                                                              ctx)))
       (values mp #`(tag #,new-imp name . rest) lifted-nss)]
      [((~and tag (~or* rename-in only-in except-in expose-in only-spaces-in except-spaces-in)) imp . rest)
       (define-values (mp new-imp lifted-nss) (loop #'imp ctx))
       (values mp #`(tag #,new-imp . rest) lifted-nss)]
      [((~and tag for-label) imp)
       (define-values (mp new-imp lifted-nss) (loop #'imp ctx))
       (values mp #`(tag #,new-imp) lifted-nss)]
      [((~and tag (~or* for-meta only-meta-in)) phase imp)
       (define-values (mp new-imp lifted-nss) (loop #'imp ctx))
       (values mp #`(tag phase #,new-imp) lifted-nss)]
      [((~and tag (~or* immediate-namespace-in)) name body)
       (define tmp-name ((make-syntax-introducer) #'name))
       (values (datum->syntax ctx
                              (list #'import-spaces #'name
                                    (list '#:all (list 'import-root #'name tmp-name '#:delay)))
                              r r)
               #f
               (list (list tmp-name #'body)))]
      [((~and tag import-dotted) orig-mp name)
       (define-values (mp new-imp lifted-nss) (loop #'orig-mp ctx))
       (values (datum->syntax ctx (list #'tag mp #'name) r r)
               new-imp
               lifted-nss)]
      [_
       (values (if ctx
                   ;; make context of import match a prefix id:
                   (datum->syntax ctx (syntax-e r) r r)
                   r)
               #f
               null)])))
