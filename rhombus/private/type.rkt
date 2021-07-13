#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "srcloc.rkt"
                     "op.rkt"))

(provide ::

         Integer)

(begin-for-syntax
  (struct infix-pattern-operator-transformer (proc)
    #:property
    prop:rhombus-infix-pattern-operator-transformer
    (lambda (self) (infix-pattern-operator-transformer-proc self)))

  (struct rhombus-type (predicate))

  (define-syntax-class :type
    (pattern id:identifier
             #:do [(define v (syntax-local-value #'id (lambda () #f)))]
             #:when (rhombus-type? v)
             #:attr predicate (rhombus-type-predicate v))))

(define-syntax ::
  (infix-pattern-operator-transformer
   (rhombus-infix-pattern-operator
    #'::
    '()
    '()
    '()
    (lambda (bindings+filter tail stx)
      (define bindings (stx-car bindings+filter))
      (define filter (stx-car (stx-cdr bindings+filter)))
      (syntax-parse tail
        [(t::type . new-tail)
         (with-syntax ([falses (for/list ([b (in-list (stx->list bindings))])
                                 #'#f)]
                       [filter filter])
           (values
            bindings
            #`(lambda (v)
                (if (t.predicate v)
                    (filter v)
                    (values #f . falses)))
            #'new-tail))]))
    #f)))

(define-syntax Integer (rhombus-type #'exact-integer?))
