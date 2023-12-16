#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     enforest/name-parse
                     "srcloc.rkt"
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
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (_::parens
                  (~or* (group _::fun-id
                               (_::parens (~and bind-g (group _ ...)))
                               op::annotate-op result-ann ...
                               (tag::block body ...))
                        (group _::fun-id
                               (_::parens (~and bind-g (group _ ...)))
                               (tag::block body ...))))
                 . tail)
        (define bind-parsed (syntax-parse #'bind-g [bind::binding #'bind.parsed]))
        (define plain-body #'(rhombus-body-at tag body ...))
        (define-values (wrapped-body static-infos)
          (cond
            [(attribute op)
             (syntax-parse (respan #`(#,group-tag result-ann ...))
               [res::annotation
                (build-annotated-expression #'form-id #'res
                                            (attribute op.check?)
                                            plain-body
                                            #'res.parsed
                                            (lambda (tmp-id)
                                              #`(raise-annotation-failure
                                                 'form-id
                                                 #,tmp-id
                                                 '#,(shrubbery-syntax->string #'res)))
                                            (lambda (form static-infos)
                                              (values form static-infos)))])]
            [else
             (values plain-body #'())]))
        (values
         (annotation-binding-form bind-parsed wrapped-body static-infos)
         #'tail)]))))
