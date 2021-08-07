#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     enforest/name-root
                     "srcloc.rkt"
                     "name-path-op.rkt")
         (only-in "implicit.rkt"
                  #%literal)
         (only-in "arithmetic.rkt"
                  [/ rhombus/]))

(provide (for-space rhombus/module-path
                    #%literal
                    (rename-out [rhombus/ /])))

(module+ for-import-export
  (provide (for-syntax make-module-path-literal-operator
                       make-module-path-/-operator
                       
                       :module-path
                       in-module-path-space
                       
                       current-module-path-context)))

(begin-for-syntax
  (property module-path-prefix-operator prefix-operator)
  (property module-path-infix-operator infix-operator)

  (define in-module-path-space (make-interned-syntax-introducer 'rhombus/module-path))

  (define current-module-path-context (make-parameter 'import))

  (define (check-module-path-result form proc)
    (unless (and (syntax? form)
                 (module-path? (syntax->datum form)))
      (raise-result-error (proc-name proc) "module-path-syntax?" form))
    form)

  (define (make-identifier-module-path id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error (current-module-path-context)
                          "not a valid module path element"
                          id))
    id)

  (define-enforest
    #:syntax-class :module-path
    #:desc "module path"
    #:operator-desc "module-path operator"
    #:in-space in-module-path-space
    #:name-path-op #f
    #:prefix-operator-ref module-path-prefix-operator-ref
    #:infix-operator-ref module-path-infix-operator-ref
    #:check-result check-module-path-result
    #:make-identifier-form make-identifier-module-path))

(define-syntax (define-module-path-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-module-path-space #'name) rhs))]))

(define-for-syntax (make-module-path-literal-operator prefix-operator)
  (prefix-operator
   #'%literal
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(_ a . tail)
        (unless (module-path? (syntax->datum #'a))
          (raise-syntax-error (current-module-path-context)
                              "not a valid module path"
                              #'a))
        (values #'a
                #'tail)]))))

(define-module-path-syntax #%literal
  (make-module-path-literal-operator module-path-prefix-operator))

(define-for-syntax (make-module-path-/-operator infix-operator)
  (infix-operator
   #'%literal
   '((default . stronger))
   'macro
   (lambda (form1 stx)
     (unless (identifier? form1)
       (raise-syntax-error (current-module-path-context)
                           "not a valid module path element"
                           form1))
     (syntax-parse stx
       [(_ a . tail)
        (unless (and (identifier? #'a)
                     (module-path? (syntax->datum #'a)))
          (raise-syntax-error (current-module-path-context)
                              "not a valid module path element"
                              #'a))
        (values (datum->syntax #'a
                               (string->symbol
                                (format "~a/~a"
                                        (syntax-e form1)
                                        (syntax-e #'a)))
                               (span-srcloc form1 #'a)
                               #'a)
                #'tail)]))
   'left))

(define-module-path-syntax rhombus/
  (make-module-path-/-operator module-path-infix-operator))
