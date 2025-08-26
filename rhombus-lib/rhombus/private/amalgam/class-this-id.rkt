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
        => (lambda (id+dp+isi+supers)
             (syntax-parse id+dp+isi+supers
               [(id dp indirect-static-infos . _)
                (define new-id
                  (let ([id (datum->syntax #'id (syntax-e #'id) #'head #'head)])
                    (if (syntax-e #'dp)
                        (wrap-static-info id
                                          #'#%dot-provider
                                          #'dp)
                        id)))
                (values (if repet?
                            (make-repetition-info (list new-id)
                                                  null
                                                  new-id
                                                  #`((#%indirect-static-info #,new-id)
                                                     . indirect-static-infos)
                                                  0)
                            (wrap-static-info*
                             new-id
                             #'indirect-static-infos))
                        #'tail)]))]
       [else
        (raise-syntax-error #f
                            "allowed only within methods"
                            #'head)])]))

(define-syntax this
  (expression-transformer (lambda (stxs) (parse-this stxs #f))))

(define-repetition-syntax this
  (repetition-transformer (lambda (stxs) (parse-this stxs #t))))
