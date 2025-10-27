#lang racket/base
(require (for-syntax racket/base
                     "class-parse.rkt"
                     "origin.rkt")
         (only-in "binding.rkt" raise-binding-failure)
         "syntax-parameter.rkt"
         "static-info.rkt")

(provide (for-syntax build-added-field-arg-definitions))

(define-for-syntax (build-added-field-arg-definitions added-fields
                                                      constructor-fields
                                                      constructor-static-infoss)
  (let loop ([added-fields added-fields]
             [args constructor-fields]
             [static-infoss constructor-static-infoss])
    (cond
      [(null? added-fields) null]
      [else
       (define added (car added-fields))
       (cons
        (with-syntax ([id (added-field-id added)]
                      [tmp-id (added-field-arg-id added)]
                      [rhs #`(with-syntax-parameters
                               #,(added-field-arg-stx-params added)
                               (let ()
                                 #,@(for/list ([arg (in-list args)]
                                               [static-infos (in-list static-infoss)])
                                      #`(define-static-info-syntax/maybe #,arg #,@static-infos))
                                 #,(added-field-arg-default added)))]
                      [converter (added-field-converter added)]
                      [annotation-str (added-field-annotation-str added)]
                      [form-id (added-field-form-id added)])
          (transfer-origins
           (added-field-annot-origins added)
           #`(define tmp-id (lambda #,args
                              (let ([id rhs])
                                #,(if (syntax-e #'converter)
                                      #`(converter id
                                                   'form-id
                                                   (lambda (who val)
                                                     (raise-binding-failure who "value" val 'annotation-str)))
                                      #'id))))))
        (loop (cdr added-fields)
              (cons (added-field-id added) args)
              (cons (added-field-static-infos added) static-infoss)))])))
