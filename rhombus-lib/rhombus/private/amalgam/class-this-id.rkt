#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/stxparam
         "provide.rkt"
         "class-this.rkt"
         "expression.rkt"
         "repetition.rkt"
         "static-info.rkt"
         "indirect-static-info-key.rkt"
         "dot-provider-key.rkt")

(provide (for-spaces (#f
                      rhombus/repet)
                     this))

(define-for-syntax (parse-this stxs repet?)
  (syntax-parse stxs
    [(head . tail)
     (cond
       [(let ([v (syntax-parameter-value #'this-id)])
          (and (not (identifier? v)) v))
        => (lambda (id+isi+supers)
             (syntax-parse id+isi+supers
               [(id all-static-infos . _)
                (define new-id
                  (datum->syntax #'id (syntax-e #'id) #'head #'head))
                (values (if repet?
                            (make-repetition-info (list new-id)
                                                  null
                                                  new-id
                                                  #`((#%indirect-static-info #,new-id)
                                                     . all-static-infos)
                                                  0)
                            (wrap-static-info*
                             new-id
                             #'all-static-infos))
                        #'tail)]))]
       [else
        (raise-syntax-error #f
                            "allowed only within methods"
                            #'head)])]))

(define-syntax this
  (expression-transformer (lambda (stxs) (parse-this stxs #f))))

(define-repetition-syntax this
  (repetition-transformer (lambda (stxs) (parse-this stxs #t))))
