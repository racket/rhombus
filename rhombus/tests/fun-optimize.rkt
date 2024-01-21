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

(define (let-named stx)
  (syntax-parse stx
    #:literals (let-values)
    [(let-values ([(name1) e]) name2)
     #:when (bound-identifier=? #'name1 #'name2)
     #'e]
    [_ stx]))

(define (extract-def mod-stx name)
  (syntax-parse mod-stx
    #:literals ([#%module-begin #%plain-module-begin]
                module
                define-values)
    [(module _ _
       (#%module-begin
        (~alt (~once (~and (define-values (f) fe)
                           (~fail #:unless (eq? (syntax-e #'f) name))))
              _)
        ...))
     #'fe]))

(define (lambda-optimized? mod-stx)
  (define def (extract-def mod-stx 'g))
  (syntax-parse def
    #:literals ([#%app #%plain-app]
                [lambda #%plain-lambda]
                #%variable-reference
                if
                variable-reference-constant?
                checked-procedure-check-and-extract)
    [(~and ge
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
  (define def (extract-def mod-stx 'f))
  (syntax-parse def
    #:literals ([#%app #%plain-app]
                procedure-reduce-keyword-arity-mask)
    [(~and fe
           (~parse (#%app procedure-reduce-keyword-arity-mask _ _ _ _)
                   (let-body #'fe)))
     #f]
    [_ #t]))

(define (lambda/kwrest-no-apply? mod-stx)
  (define (no-apply? proc-stx)
    (syntax-parse proc-stx
      #:literals ([#%app #%plain-app]
                  case-lambda
                  apply)
      [(case-lambda
         [_ (#%app _ ...)]
         ...
         [_ (#%app apply _ ...)]
         [_ (#%app _ ...)]
         ...)
       #f]
      [_ #t]))
  (define def (extract-def mod-stx 'f))
  (syntax-parse def
    #:literals ([#%app #%plain-app]
                procedure-reduce-keyword-arity-mask
                make-keyword-procedure)
    [(~and fe
           (~parse (~or (#%app procedure-reduce-keyword-arity-mask inner _ _ _)
                        inner)
                   (let-body #'fe))
           (~parse (#%app make-keyword-procedure kw-proc proc)
                   #'inner))
     (and (no-apply? (let-named #'kw-proc))
          (no-apply? (let-named #'proc)))]
    [_ #f]))

(check-pred lambda/kwrest-not-reduced?
            @get-module['fun/kwrest1]{
 fun f(~& kwrest):
   kwrest
 })

(let ([mod-stx @get-module['fun/kwrest2]{
        fun f(& _, ~& kwrest):
          kwrest
        }])
  (check-pred lambda/kwrest-not-reduced? mod-stx)
  (check-pred lambda/kwrest-no-apply? mod-stx))

(let ([mod-stx @get-module['fun/kwrest3]{
        fun f(_, & _, ~& kwrest):
          kwrest
        }])
  (check-pred lambda/kwrest-not-reduced? mod-stx)
  (check-pred lambda/kwrest-no-apply? mod-stx))

(define ((case-lambda-optimized? match-case) mod-stx)
  (define def (extract-def mod-stx 'f))
  (syntax-parse def
    #:literals (case-lambda)
    [(~and fe
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
  (lambda/kwrest-not-reduced? mod-stx))

(define (case-lambda/kwrest-no-apply? mod-stx)
  (lambda/kwrest-no-apply? mod-stx))

(check-pred case-lambda/kwrest-not-reduced?
            @get-module['case-fun/kwrest1]{
 fun
 | f(~& kwrest): kwrest
 })

(let ([mod-stx @get-module['case-fun/kwrest2]{
        fun
        | f(& rest): rest
        | f(~& kwrest): kwrest
        }])
  (check-pred case-lambda/kwrest-not-reduced? mod-stx)
  (check-pred case-lambda/kwrest-no-apply? mod-stx))

(let ([mod-stx @get-module['case-fun/kwrest3]{
        fun
        | f(_, & rest): rest
        | f(~& kwrest): kwrest
        }])
  (check-pred case-lambda/kwrest-not-reduced? mod-stx)
  (check-pred case-lambda/kwrest-no-apply? mod-stx))
