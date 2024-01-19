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

(define (letrec-body stx)
  (syntax-parse stx
    #:literals (letrec-values)
    [(letrec-values _ body) (let-body #'body)]
    [_ stx]))

(define (let-named stx)
  (syntax-parse stx
    #:literals (let-values)
    [(let-values ([(name1) e]) name2)
     #:when (bound-identifier=? #'name1 #'name2)
     #'e]
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

(define (lambda/kwrest-not-reduced? mod-stx)
  (syntax-parse mod-stx
    #:datum-literals (f)
    #:literals ([#%module-begin #%plain-module-begin]
                [#%app #%plain-app]
                module define-values case-lambda
                make-keyword-procedure)
    [(~and (module _ _
             (#%module-begin
              (~alt (~once (define-values (f) fe))
                    _)
              ...))
           (~parse (#%app make-keyword-procedure _ inner)
                   (letrec-body #'fe))
           (~parse (case-lambda . _)
                   (let-named #'inner)))
     #t]
    [_ #f]))

(check-pred lambda/kwrest-not-reduced?
            @get-module['fun/kwrest1]{
 fun f(~& kwrest):
   kwrest
 })

(check-pred lambda/kwrest-not-reduced?
            @get-module['fun/kwrest2]{
 fun f(& _, ~& kwrest):
   kwrest
 })

(check-pred lambda/kwrest-not-reduced?
            @get-module['fun/kwrest3]{
 fun f(_, & _, ~& kwrest):
   kwrest
 })

(define ((case-lambda-optimized? match-case) mod-stx)
  (syntax-parse mod-stx
    #:datum-literals (f)
    #:literals ([#%module-begin #%plain-module-begin]
                module define-values case-lambda)
    [(~and (module _ _
             (#%module-begin
              (~alt (~once (define-values (f) fe))
                    _)
              ...))
           (~parse (~and fn (case-lambda . _))
                   (let-body (let-named #'fe))))
     (match-case #'fn)]
    [_ #f]))

(check-pred (case-lambda-optimized?
             (lambda (stx)
               (syntax-parse stx
                 [(_ [(_) . _]) #t]
                 [_ #f])))
            @get-module['case-fun1]{
 fun
 | f(a): a
 })

(check-pred (case-lambda-optimized?
             (lambda (stx)
               (syntax-parse stx
                 [(_ [(_) . _] [(_ _) . _]) #t]
                 [_ #f])))
            @get-module['case-fun2]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 })

;; FIXME these currently aren't as optimized as they can be
(check-pred (case-lambda-optimized?
             (lambda (stx)
               (syntax-parse stx
                 [(_ #;[(_) . _] #;[(_ _) . _] [(_ . _) . _]) #t]
                 [_ #f])))
            @get-module['case-fun3]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, & bs): values(a, & bs)
 })

(check-pred (case-lambda-optimized?
             (lambda (stx)
               (syntax-parse stx
                 [(_ #;[(_) . _] #;[(_ _) . _] [(_ . _) . _]) #t]
                 [_ #f])))
            @get-module['case-fun4]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, ...): values(a, b, ...)
 })

(check-pred (case-lambda-optimized?
             (lambda (stx)
               (syntax-parse stx
                 [(_ [(_) . _] #;[(_ _) . _] [(_ _ . _) . _]) #t]
                 [_ #f])))
            @get-module['case-fun5]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, & cs): values(a, b, & cs)
 })

(check-pred (case-lambda-optimized?
             (lambda (stx)
               (syntax-parse stx
                 [(_ [(_) . _] #;[(_ _) . _] [(_ _ . _) . _]) #t]
                 [_ #f])))
            @get-module['case-fun6]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, c, ...): values(a, b, c, ...)
 })

(check-pred (case-lambda-optimized?
             (lambda (stx)
               (syntax-parse stx
                 [(_ [(_) . _] [(_ _) . _] [(_ _ _ . _) . _]) #t]
                 [_ #f])))
            @get-module['case-fun7]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, c, & ds): values(a, b, c, & ds)
 })

(check-pred (case-lambda-optimized?
             (lambda (stx)
               (syntax-parse stx
                 [(_ [(_) . _] [(_ _) . _] [(_ _ _ . _) . _]) #t]
                 [_ #f])))
            @get-module['case-fun8]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, c, d, ...): values(a, b, c, d, ...)
 })

(define (case-lambda/kwrest-not-reduced? mod-stx)
  (syntax-parse mod-stx
    #:datum-literals (f)
    #:literals ([#%module-begin #%plain-module-begin]
                [#%app #%plain-app]
                module define-values case-lambda
                make-keyword-procedure)
    [(~and (module _ _
             (#%module-begin
              (~alt (~once (define-values (f) fe))
                    _)
              ...))
           (~parse (#%app make-keyword-procedure _ inner)
                   (let-body #'fe))
           (~parse (case-lambda . _)
                   (let-named #'inner)))
     #t]
    [_ #f]))

(check-pred case-lambda/kwrest-not-reduced?
            @get-module['case-fun/kwrest1]{
 fun
 | f(~& kwrest): kwrest
 })

(check-pred case-lambda/kwrest-not-reduced?
            @get-module['case-fun/kwrest2]{
 fun
 | f(& rest): rest
 | f(~& kwrest): kwrest
 })

(check-pred case-lambda/kwrest-not-reduced?
            @get-module['case-fun/kwrest3]{
 fun
 | f(_, & rest): rest
 | f(~& kwrest): kwrest
 })
