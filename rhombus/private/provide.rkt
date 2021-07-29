#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest
                     enforest/operator
                     enforest/property
                     enforest/proc-name)
         "declaration.rkt")

(provide (rename-out [rhombus-provide provide])

         (for-space rhombus/provide rename))

(begin-for-syntax
  (property provide-prefix-operator prefix-operator)
  (property provide-infix-operator infix-operator)

  (define in-provide-space (make-interned-syntax-introducer 'rhombus/provide))

  (define (check-provide-result form proc)
    (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
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
    provide-prefix-operator-ref provide-infix-operator-ref
    check-provide-result
    make-identifier-provide))

(define-syntax rhombus-provide
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block p::provide ...))
        #`((provide p.parsed ...))]))))

(define-syntax (define-provide-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-provide-space #'name) rhs))]))

(define-provide-syntax rename
  (provide-prefix-operator
   #'rename
   '((default . stronger))
   #t
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block (group int:identifier (~datum to) ext:identifier)
                  ...)
           . tail)
        (values #`(rename-out [int ext] ...)
                #'tail)]))))
