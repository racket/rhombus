#lang at-exp racket/base
(require racket/symbol
         syntax/modread
         syntax/parse/pre
         rackunit)

(define (get-module name . strs)
  (define mod-stx
    (check-module-form
     (with-module-reading-parameterization
       (lambda ()
         (define mod-str (apply string-append "#lang rhombus\n" strs))
         (define in (open-input-string mod-str))
         (port-count-lines! in)
         (read-syntax name in)))
     name
     (symbol->immutable-string name)))
  (parameterize ([compile-enforce-module-constants #t])
    (expand-syntax mod-stx)))

(define (let-body stx)
  (syntax-parse stx
    #:literals (let-values)
    [(let-values _ body) (let-body #'body)]
    [_ stx]))

(define (lambda-optimized? mod-stx)
  (syntax-parse mod-stx
    #:datum-literals (g)
    #:literals ([#%module-begin #%plain-module-begin]
                [#%app #%plain-app]
                [lambda #%plain-lambda]
                #%variable-reference
                module define-values if
                variable-reference-constant?
                checked-procedure-check-and-extract)
    [(~and (module _ _
             (#%module-begin
              (~alt (~once (define-values (g) ge))
                    _)
              ...))
           (~parse (lambda () gb)
                   (let-body #'ge))
           (~parse (if (#%app variable-reference-constant? (#%variable-reference _))
                       (#%app . _)
                       (#%app (#%app checked-procedure-check-and-extract . _) . _))
                   (let-body #'gb)))
     #t]
    [_ #f]))

(check-pred lambda-optimized?
            @get-module['fun1]{
 fun f(x, ~b):
   b

 fun g():
   f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun2]{
 fun f(x, ~b, & _):
   b

 fun g():
   f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun3]{
 fun f(x, ~b, _, ...,):
   b

 fun g():
   f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun4]{
 fun f(x, ~b = 1):
   b

 fun g():
   f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun5]{
 fun f(x, ~b = 1, & _):
   b

 fun g():
   f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun6]{
 fun f(x, ~b = 1, _, ...,):
   b

 fun g():
   f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun1/namespaced]{
 namespace prefix
 fun prefix.f(x, ~b):
   b

 fun g():
   prefix.f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun2/namespaced]{
 namespace prefix
 fun prefix.f(x, ~b, & _):
   b

 fun g():
   prefix.f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun3/namespaced]{
 namespace prefix
 fun prefix.f(x, ~b, _, ...,):
   b

 fun g():
   prefix.f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun4/namespaced]{
 namespace prefix
 fun prefix.f(x, ~b = 1):
   b

 fun g():
   prefix.f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun5/namespaced]{
 namespace prefix
 fun prefix.f(x, ~b = 1, & _):
   b

 fun g():
   prefix.f(1, ~b: 2)
 })

(check-pred lambda-optimized?
            @get-module['fun6/namespaced]{
 namespace prefix
 fun prefix.f(x, ~b = 1, _, ...,):
   b

 fun g():
   prefix.f(1, ~b: 2)
 })

(define (case-lambda-optimized? mod-stx)
  (syntax-parse mod-stx
    #:datum-literals (f)
    #:literals ([#%module-begin #%plain-module-begin]
                module define-values case-lambda)
    [(~and (module _ _
             (#%module-begin
              (~alt (~once (define-values (f) fe))
                    _)
              ...))
           (~parse (case-lambda . _)
                   (let-body #'fe)))
     #t]
    [_ #f]))

(check-pred case-lambda-optimized?
            @get-module['case-fun1]{
 fun
 | f(a): a
 })

(check-pred case-lambda-optimized?
            @get-module['case-fun2]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 })

(check-pred case-lambda-optimized?
            @get-module['case-fun3]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, & bs): values(a, & bs)
 })

(check-pred case-lambda-optimized?
            @get-module['case-fun4]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, ...): values(a, b, ...)
 })
