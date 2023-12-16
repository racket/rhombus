#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     enforest/name-parse
                     "srcloc.rkt"
                     "with-syntax.rkt"
                     "tag.rkt")
         (submod "annotation.rkt" for-class)
         "annotation-operator.rkt"
         "parse.rkt"
         "parens.rkt"
         (only-in "function.rkt" fun)
         (only-in "expression.rkt" expr-quote))

(provide (for-space rhombus/annot

                    converting))

(begin-for-syntax
  (define-syntax-class :fun-id
    #:attributes (name)
    #:description "the literal `fun`"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? #'name (expr-quote fun)))))

(define-annotation-syntax converting
  (annotation-prefix-operator
   (annot-quote converting)
   '((default . stronger))
   'macro
   (lambda (stx)
     (define (parse form-id bind-group result-ann plain-body check? tail)
       (with-syntax-parse ([bind::binding bind-group])
         (define-values (wrapped-body static-infos)
           (cond
             [result-ann
              (with-syntax-parse ([res::annotation result-ann])
                (build-annotated-expression form-id #'res
                                            check?
                                            plain-body
                                            #'res.parsed
                                            (lambda (tmp-id)
                                              #`(raise-annotation-failure 'form-id
                                                                          #,tmp-id
                                                                          '#,(shrubbery-syntax->string #'res)))
                                            values))]
             [else (values plain-body #'())]))
         (values
          (annotation-binding-form
           #'bind.parsed
           wrapped-body
           static-infos)
          tail)))
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (_::parens (group _::fun-id
                                   (_::parens (group arg ...))
                                   op::annotate-op result-ann ...
                                   (tag::block body ...)))
                 . tail)
        (parse #'form-id
               #'(group arg ...)
               (respan #`(#,group-tag result-ann ...))
               #'(rhombus-body-at tag body ...)
               (attribute op.check?)
               #'tail)]
       [(form-id (_::parens (group _::fun-id
                                   (_::parens (group arg ...))
                                   (tag::block body ...)))
                 . tail)
        (parse #'form-id
               #'(group arg ...)
               #f
               #'(rhombus-body-at tag body ...)
               #f
               #'tail)]))))
