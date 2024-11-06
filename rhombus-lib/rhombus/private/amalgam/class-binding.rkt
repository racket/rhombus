#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
                     enforest/syntax-local
                     (only-in enforest/operator operator-proc)
                     "srcloc.rkt")
         "binding.rkt"
         "dot-provider-key.rkt"
         (submod "bind-macro.rkt" for-class)
         "composite.rkt"
         "parens.rkt"
         (submod "boolean-pattern.rkt" for-class)
         "class-transformer.rkt"
         (submod "dot.rkt" for-dot-provider)
         "dotted-sequence-parse.rkt")

(provide (for-syntax build-class-binding-form))

(define-for-syntax (build-class-binding-form super binding-rhs
                                             exposed-internal-id intro
                                             names)
  (with-syntax ([(name name-extends tail-name
                       name-instance name?
                       indirect-static-infos dot-providers
                       constructor-name-fields constructor-public-name-fields super-name-fields
                       constructor-field-static-infoss constructor-public-field-static-infoss super-field-static-infoss
                       field-keywords public-field-keywords super-field-keywords)
                 names])
    (define (make-binding-transformer no-super? name-fields static-infoss keywords)
      (with-syntax ([(constructor-name-field ...) name-fields]
                    [(constructor-field-static-infos ...) static-infoss]
                    [(field-keyword ...) keywords]
                    [(super-name-field ...) (if no-super? '() #'super-name-fields)]
                    [(super-field-static-infos ...) (if no-super? '() #'super-field-static-infoss)]
                    [(super-field-keyword ...) (if no-super? '() #'super-field-keywords)])
        #`(binding-transformer
           (lambda (stx)
             (composite-binding-transformer stx
                                            '#,(symbol->immutable-string (syntax-e #'name))
                                            (quote-syntax name?)
                                            #:static-infos (quote-syntax ((#%dot-provider dot-providers)
                                                                          . indirect-static-infos))
                                            (list (quote-syntax super-name-field) ...
                                                  (quote-syntax constructor-name-field) ...)
                                            #:keywords '(super-field-keyword ... field-keyword ...)
                                            (list (quote-syntax super-field-static-infos) ...
                                                  (quote-syntax constructor-field-static-infos) ...)
                                            #:accessor->info? #t)))))
    (append
     (if exposed-internal-id
         (list
          #`(define-binding-syntax #,exposed-internal-id
              #,(make-binding-transformer #t
                                          #'constructor-name-fields
                                          #'constructor-field-static-infoss
                                          #'field-keywords)))
         null)
     (cond
       [binding-rhs
        (cond
          [(eq? '#:none (syntax-e binding-rhs))
           null]
          [else
           (list
            (build-syntax-definition/maybe-extension
             'rhombus/bind #'name #'name-extends
             (if (eq? '#:error (syntax-e binding-rhs))
                 #'no-binding-transformer
                 (wrap-class-transformer #'name #'tail-name (intro binding-rhs) #'make-binding-prefix-operator "class"))))])]
       [else
        (list
         (build-syntax-definition/maybe-extension
          'rhombus/bind #'name #'name-extends
          (make-binding-transformer #f
                                    #'constructor-public-name-fields
                                    #'constructor-public-field-static-infoss
                                    #'public-field-keywords)))]))))

(define-for-syntax no-binding-transformer
  (binding-transformer
   (lambda (stx)
     (raise-syntax-error #f "cannot be used as a binding pattern" stx))))
