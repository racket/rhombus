#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/stxparam
         "class-this.rkt"
         "expression.rkt"
         "static-info.rkt"
         "dot-provider-key.rkt")

(provide this)

(define-syntax this
  (expression-transformer
   (lambda (stxs)
     (syntax-parse stxs
       [(head . tail)
        (cond
          [(let ([v (syntax-parameter-value #'this-id)])
             (and (not (identifier? v)) v))
           => (lambda (id+dp+isi+supers)
                (syntax-parse id+dp+isi+supers
                  [(id dp indirect-static-infos . _)
                   (values (wrap-static-info*
                            (let ([id (datum->syntax #'id (syntax-e #'id) #'head #'head)])
                              (if (syntax-e #'dp)
                                  (wrap-static-info id
                                                    #'#%dot-provider
                                                    #'dp)
                                  id))
                            #'indirect-static-infos)
                           #'tail)]))]
          [else
           (raise-syntax-error #f
                               "allowed only within methods"
                               #'head)])]))))
