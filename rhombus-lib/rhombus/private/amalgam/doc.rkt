#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/strip-context
                     "dotted-sequence.rkt")
         (submod "module.rkt" for-module+)
         "parens.rkt")

(provide (for-syntax maybe-add-doc))

(define-for-syntax (maybe-add-doc doc names prototypes doc-kw-stx orig-stx defns)
  (cond
    [(and doc
          (eq? 'module (syntax-local-context)))
     (define id (build-dot-symbol (syntax->list names)))
     (define gs
       (syntax-parse doc
         [((tag::block g ...)) #'(g ...)]
         [() #'()]
         [_ (raise-syntax-error #f
                                "expected nothing or a block after `~doc`"
                                orig-stx
                                doc)]))
     (cons
      #`(rhombus-module+
         #,(datum->syntax #f 'doc doc-kw-stx)
         #:orig #,orig-stx
         #:language (lib "rhombus/doc.rhm")
         (group export #,id)
         (group def #,id
                (op =)
                DocSpec
                (parens (group List
                               (parens
                                #,@(for/list ([prototype (in-list prototypes)])
                                     #`(group Syntax (op |.|) literal (quotes #,(strip-context prototype))))))
                        (group List
                               (parens
                                #,@(for/list ([g (in-list (syntax->list gs))])
                                     #`(group Syntax (op |.|) literal (quotes #,g))))))))
      defns)]
    [else defns]))
