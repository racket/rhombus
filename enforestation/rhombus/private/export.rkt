#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     "name-path-op.rkt")
         "declaration.rkt")

(provide export

         (for-space rhombus/export
                    rename
                    operator))

(begin-for-syntax
  (property export-prefix-operator prefix-operator)
  (property export-infix-operator infix-operator)

  (define in-export-space (make-interned-syntax-introducer 'rhombus/export))

  (define (check-export-result form proc)
    (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
    form)

  (define (make-identifier-export id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error 'export
                          "not a valid module path element"
                          id))
    id)

  (define-enforest export-enforest export-enforest-step
    :export :export-prefix-op+form+tail :export-infix-op+form+tail
    "export" "export operator"
    in-export-space
    name-path-op export-prefix-operator-ref export-infix-operator-ref
    check-export-result
    make-identifier-export))

(define-syntax export
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block p::export ...))
        #`((provide p.parsed ...))]))))

(define-syntax (define-export-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-export-space #'name) rhs))]))

(define-export-syntax rename
  (export-prefix-operator
   #'rename
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block (group int:identifier #:to ext:identifier)
                  ...)
           . tail)
        (values #`(rename-out [int ext] ...)
                #'tail)]))))

(define-export-syntax operator
  (export-prefix-operator
   #'rename
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block op)
       [(_ (block (group (op name) ...) ...)
           . tail)
        (values #`(combine-out name ... ...)
                #'tail)]))))

