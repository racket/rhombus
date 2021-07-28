#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "op.rkt"
                     "transformer.rkt"
                     "enforest.rkt"
                     "property.rkt")
         (only-in "core-implicit.rkt"
                  #%literal))

(provide (rename-out [rhombus-require require])

         (for-space rhombus/require
                    #%literal
                    rename))

(begin-for-syntax
  (property require-prefix-operator prefix-operator)
  (property require-infix-operator infix-operator)

  (define in-require-space (make-interned-syntax-introducer 'rhombus/require))

  (define (check-require-result form proc)
    form)

  (define (make-identifier-require id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error 'require
                          "not a valid module path element"
                          id))
    id)

  (define-enforest require-enforest require-enforest-step
    :require :require-prefix-op+form+tail :require-infix-op+form+tail
    "require" "require operator"
    in-require-space
    require-prefix-operator-ref require-infix-operator-ref
    check-require-result
    make-identifier-require))

(define-syntax rhombus-require
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block r::require ...))
        #`((require r.expanded ...))]))))

(define-syntax (define-require-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-require-space #'name) rhs))]))

(define-require-syntax #%literal
  (require-prefix-operator
   #'%literal
   '((default . stronger))
   #t ; transformer
   (lambda (stx)
     (syntax-parse stx
       [(a . tail)
        (unless (module-path? (syntax->datum #'a))
          (raise-syntax-error 'require
                              "not a valid module path"
                              #'a))
        (values #'a
                #'tail)]))))

(define-require-syntax rename
  (require-infix-operator
   #'rename
   '((default . stronger))
   #t
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block (group int:identifier (~datum to) ext:identifier)
                  ...)
           . tail)
        (values #`(rename-in #,req [int ext] ...)
                #'tail)]))
   #f))
