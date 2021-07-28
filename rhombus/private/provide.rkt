#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "op.rkt"
                     "transformer.rkt"
                     "enforest.rkt"
                     "property.rkt")
         (only-in "core-implicit.rkt"
                  #%literal))

(provide (rename-out [rhombus-provide provide]))

(begin-for-syntax
  (property provide-prefix-operator prefix-operator)
  (property provide-infix-operator infix-operator)

  (property provide-transformer transformer)

  (define in-provide-space (make-interned-syntax-introducer 'rhombus/provide))

  (define (check-provide-result form proc)
    form)

  (define (make-identifier-provide id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error 'provide
                          "not a valid module path element"
                          id))
    id)

  (define-enforest provide-enforest provide-enforest-step
    :provide :provide-prefix-op+form+tail :provide-infix-op+form+tail
    "provide" "provide operator"
    in-provide-space
    provide-transformer-ref provide-prefix-operator-ref provide-infix-operator-ref
    check-provide-result
    make-identifier-provide))

(define-syntax rhombus-provide
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block p::provide ...))
        #`((provide p.expanded ...))]))))

(define-syntax (define-provide-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-provide-space #'name) rhs))]))
