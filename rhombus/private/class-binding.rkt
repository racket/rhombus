#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     (only-in enforest/operator operator-proc)
                     "srcloc.rkt"
                     "class-parse.rkt")
         "binding.rkt"
         "binding-syntax.rkt"
         (submod "binding-syntax.rkt" for-class)
         "composite.rkt"
         "parens.rkt"
         (submod "boolean-pattern.rkt" for-class)
         (for-syntax "class-transformer.rkt")
         (submod "dot.rkt" for-dot-provider))

(provide (for-syntax build-class-binding-form))

(define-for-syntax (build-class-binding-form super binding-id options
                                             exposed-internal-id intro
                                             names)
  (with-syntax ([(name name-instance name?
                       [constructor-name-field ...] [super-name-field ...]
                       [constructor-field-static-infos ...] [super-field-static-infos ...]
                       [field-keyword ...] [super-field-keyword ...])
                 names])
    (with-syntax ([core-bind-name (if (hash-ref options 'binding #f)
                                      (car (generate-temporaries #'(name)))
                                      #'name)])
      (append
       (list
        #`(define-binding-syntax core-bind-name
            (binding-transformer
             (quote-syntax name)
             #,(if (and super
                        binding-id)
                   #`(make-curried-binding-transformer (quote-syntax #,(class-desc-binding-id super))
                                                       #,(symbol->string (syntax-e #'name))
                                                       (quote-syntax name?)
                                                       #:static-infos (quote-syntax ((#%dot-provider name-instance)))
                                                       (list (quote-syntax constructor-name-field) ...)
                                                       #:keywords '(field-keyword ...)
                                                       (list (quote-syntax constructor-field-static-infos) ...))
                   #`(make-composite-binding-transformer #,(symbol->string (syntax-e #'name))
                                                         (quote-syntax name?)
                                                         #:static-infos (quote-syntax ((#%dot-provider name-instance)))
                                                         (list (quote-syntax super-name-field) ...
                                                               (quote-syntax constructor-name-field) ...)
                                                         #:keywords '(super-field-keyword ... field-keyword ...)
                                                         (list (quote-syntax super-field-static-infos) ...
                                                               (quote-syntax constructor-field-static-infos) ...)
                                                         #:accessor->info? #t)))))
       (if exposed-internal-id
           (list
            #`(define-binding-syntax #,exposed-internal-id (make-rename-transformer (quote-syntax core-bind-name))))
           null)
       (cond
         [(hash-ref options 'binding #f)
          => (lambda (bind)
               (list
                #`(define-binding-syntax #,(intro binding-id) (make-rename-transformer
                                                               (quote-syntax #,(in-binding-space #'core-bind-name))))
                #`(define-binding-syntax name
                    (wrap-class-transformer name #,(intro (cadr bind)) make-binding-prefix-operator))))]
         [else null])))))

(define-for-syntax (make-curried-binding-transformer super-binding-id
                                                     constructor-str predicate accessors static-infoss
                                                     #:static-infos static-infos
                                                     #:keywords keywords)
  (define t
    (make-composite-binding-transformer constructor-str predicate accessors static-infoss
                                        #:static-infos static-infos
                                        #:keywords keywords
                                        #:accessor->info? #t))
  (cond
    [super-binding-id
     (define p-t (operator-proc
                  (syntax-local-value* (in-binding-space super-binding-id) binding-prefix-operator-ref)))
     (lambda (tail)
       (syntax-parse tail
         [(form-id p-term (tag::parens g ...) . new-tail)
          (define stx (no-srcloc #'(form-id p-term (tag g ...))))
          (define-values (p-binding p-tail) (p-t #'(form-id p-term)))
          (define-values (binding c-tail) (t #'(form-id (tag g ...)) #f stx))
          (values (make-and-binding p-binding binding)
                  #'new-tail)]))]
    [else t]))
