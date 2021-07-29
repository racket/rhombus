#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest/syntax-local
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt")

(provide ::

         Integer

         (for-syntax rhombus-type
                     rhombus-typed
                     rhombus-type-property
                     rhombus-syntax-local-type
                     in-type-space)

         define-type-syntax)

(begin-for-syntax
  (struct rhombus-type (predicate))

  (struct rhombus-typed (type-stx proc)
    #:property prop:procedure (struct-field-index proc))

  (define rhombus-type-property (gensym))

  (define in-type-space (make-interned-syntax-introducer 'rhombus/type))

  (define-syntax-class :type
    (pattern id:identifier
             #:do [(define v (syntax-local-value* (in-type-space #'id) rhombus-type?))]
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
  (binding-infix-operator
   #'::
   '((default . weaker))
   #t ; transformer
   (lambda (form tail)
     (syntax-parse tail
       [(op t::type . new-tail)
        #:with left::binding-form form
        (define num-vars (length (syntax->list #'left.var-ids)))
        (with-syntax ([falses (for/list ([i (in-range num-vars)])
                                #'#f)])
          (define-values (var-ids new-def)
            (cond
              [(and (= 1 num-vars)
                    (syntax-parse #'left.post-defn [(begin) #t] [_ #f]))
               (define tmp-id (car (generate-temporaries #'left.var-ids)))
               (values (list tmp-id)
                       #`(define-syntaxes left.var-ids
                           (make-typed-identifier (quote-syntax #,tmp-id) (quote-syntax t))))]
              [else (values #'left.var-ids
                            #'(begin))]))
          (values
           (binding-form var-ids
                         #`(lambda (v)
                             (if (t.predicate v)
                                 (left.check-proc-expr v)
                                 (values #f . falses)))
                         #`(begin
                             #,new-def
                             left.post-defn))
           #'new-tail))]))
   #f))

(define-syntax Integer (rhombus-type #'exact-integer?))

(define-syntax (define-type-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-type-space #'id)
         rhs)]))
