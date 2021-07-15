#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "srcloc.rkt"
                     "op.rkt"
                     "syntax-local.rkt")
         "parse.rkt")

(provide ::

         Integer

         (for-syntax rhombus-type
                     rhombus-typed
                     rhombus-type-property
                     rhombus-syntax-local-type))

(begin-for-syntax
  (struct rhombus-type (predicate))

  (struct rhombus-typed (type-stx proc)
    #:property prop:procedure (struct-field-index proc))

  (define rhombus-type-property (gensym))

  (define-syntax-class :type
    (pattern id:identifier
             #:do [(define v (syntax-local-value* #'id rhombus-type?))]
             #:when (rhombus-type? v)
             #:attr predicate (rhombus-type-predicate v)))

  (define (make-typed-identifier id type-stx)
    (rhombus-typed
     type-stx
     (lambda (stx)
       (define (get-id stx)
         (syntax-property (datum->syntax id
                                         (syntax-e id)
                                         stx)
                          rhombus-type-property
                          type-stx))
       (syntax-parse stx
         [_:identifier (get-id stx)]
         [(rator rand ...) (datum->syntax (quote-syntax here)
                                          (cons (get-id #'rator) #'(rand ...))
                                          stx
                                          stx)]))))

  (define (rhombus-syntax-local-type e)
    (cond
      [(syntax-property e rhombus-type-property)
       => (lambda (type-stx) type-stx)]
      [(identifier? e)
       (define v (syntax-local-value* e rhombus-typed?))
       (and (rhombus-typed? v)
            (rhombus-typed-type-stx v))]
      [else #f])))

(define-syntax ::
  (rhombus-infix-binding-operator-transformer
   #'::
   '((default . weaker))
   #f
   (lambda (form tail)
     (syntax-parse tail
       [(op t::type . new-tail)
        #:with left::binding-form form
        (with-syntax ([falses (for/list ([b (in-list (stx->list #'left.variable-ids))])
                                #'#f)])
          (define-values (var-ids stx-ids stx-form)
            (cond
              [(= 1 (length (stx->list #'left.variable-ids)))
               (define tmp-id (car (generate-temporaries #'left.variable-ids)))
               (values (list tmp-id)
                       #'left.variable-ids
                       #`(make-typed-identifier (quote-syntax #,tmp-id) (quote-syntax t)))]
              [else (values #'left.variable-ids
                            #'left.syntax-ids
                            #'left.syntax-form)]))
          (values
           var-ids
           #`(lambda (v)
               (if (t.predicate v)
                   (left.matcher-form v)
                   (values #f . falses)))
           stx-ids
           stx-form
           #'new-tail))]))))

(define-syntax Integer (rhombus-type #'exact-integer?))
