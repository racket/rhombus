#lang at-exp racket/base
(require (for-syntax racket/base
                     racket/syntax-srcloc
                     syntax/parse/pre)
         racket/symbol
         raco/testing
         syntax/modread
         syntax/parse/pre)

(define (get-module name mod-str)
  (define mod-stx
    (check-module-form
     (with-module-reading-parameterization
       (lambda ()
         (define in (open-input-string mod-str))
         (port-count-lines! in)
         (read-syntax name in)))
     name
     (symbol->immutable-string name)))
  (parameterize ([compile-enforce-module-constants #t])
    (expand-syntax mod-stx)))

(define-syntax (check-module stx)
  (syntax-parse stx
    [(_ name:identifier
        (~seq #:is pred)
        ...
        str:string ...)
     (define error-str
       (string-append (srcloc->string (syntax-srcloc stx)) ": module content does not satisfy predicate"
                      "\n  name: ~s"
                      "\n  predicate: ~s"))
     (define module-str
       (apply string-append "#lang rhombus\n" (syntax->datum #'(str ...))))
     #`(let ([mod-stx (get-module 'name '#,module-str)])
         (let ([ok? (pred mod-stx)])
           (test-log! ok?)
           (unless ok?
             (eprintf '#,error-str 'name pred)))
         ...)]))

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

@check-module[fun1
              #:is lambda-optimized?]{
 fun f(x, ~b):
   b

 fun g():
   f(1, ~b: 2)
}

@check-module[fun2
              #:is lambda-optimized?]{
 fun f(x, ~b, & _):
   b

 fun g():
   f(1, ~b: 2)
}

@check-module[fun3
              #:is lambda-optimized?]{
 fun f(x, ~b, _, ...,):
   b

 fun g():
   f(1, ~b: 2)
}

@check-module[fun4
              #:is lambda-optimized?]{
 fun f(x, ~b = 1):
   b

 fun g():
   f(1, ~b: 2)
}

@check-module[fun5
              #:is lambda-optimized?]{
 fun f(x, ~b = 1, & _):
   b

 fun g():
   f(1, ~b: 2)
}

@check-module[fun6
              #:is lambda-optimized?]{
 fun f(x, ~b = 1, _, ...,):
   b

 fun g():
   f(1, ~b: 2)
}

@check-module[fun1/namespaced
              #:is lambda-optimized?]{
 namespace prefix
 fun prefix.f(x, ~b):
   b

 fun g():
   prefix.f(1, ~b: 2)
}

@check-module[fun2/namespaced
              #:is lambda-optimized?]{
 namespace prefix
 fun prefix.f(x, ~b, & _):
   b

 fun g():
   prefix.f(1, ~b: 2)
}

@check-module[fun3/namespaced
              #:is lambda-optimized?]{
 namespace prefix
 fun prefix.f(x, ~b, _, ...,):
   b

 fun g():
   prefix.f(1, ~b: 2)
}

@check-module[fun4/namespaced
              #:is lambda-optimized?]{
 namespace prefix
 fun prefix.f(x, ~b = 1):
   b

 fun g():
   prefix.f(1, ~b: 2)
}

@check-module[fun5/namespaced
              #:is lambda-optimized?]{
 namespace prefix
 fun prefix.f(x, ~b = 1, & _):
   b

 fun g():
   prefix.f(1, ~b: 2)
}

@check-module[fun6/namespaced
              #:is lambda-optimized?]{
 namespace prefix
 fun prefix.f(x, ~b = 1, _, ...,):
   b

 fun g():
   prefix.f(1, ~b: 2)
}

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
           (~parse (~or* (#%app procedure-reduce-keyword-arity-mask inner _ _ _)
                         inner)
                   (let-body #'fe))
           (~parse (#%app make-keyword-procedure kw-proc proc)
                   #'inner))
     (and (no-apply? (let-named #'kw-proc))
          (no-apply? (let-named #'proc)))]
    [_ #f]))

@check-module[fun/kwrest1
              #:is lambda/kwrest-not-reduced?]{
 fun f(~& kwrest):
   kwrest
}

@check-module[fun/kwrest2
              #:is lambda/kwrest-not-reduced?
              #:is lambda/kwrest-no-apply?]{
 fun f(& _, ~& kwrest):
   kwrest
}

@check-module[fun/kwrest3
              #:is lambda/kwrest-not-reduced?
              #:is lambda/kwrest-no-apply?]{
 fun f(_, & _, ~& kwrest):
   kwrest
}

(define ((lambda/kwrest-opt-no-destructure? num-of-kws) mod-stx)
  (define def (extract-def mod-stx 'f))
  (syntax-parse def
    #:literals ([#%app #%plain-app]
                make-keyword-procedure
                case-lambda)
    [(~and fe
           (~parse (#%app make-keyword-procedure kw-proc proc)
                   (let-body #'fe))
           (~parse (case-lambda
                     [(_ _ . _) (#%app _ kw-arg ...)]
                     ...)
                   (let-named #'kw-proc))
           (~parse (case-lambda
                     [_ (#%app _ arg ...)]
                     ...)
                   (let-named #'proc)))
     (for/and ([kw-args (in-list (syntax->list #'((kw-arg ...) ...)))]
               [args (in-list (syntax->list #'((arg ...) ...)))])
       (eqv? (- (length (syntax->list args))
                (length (syntax->list kw-args)))
             num-of-kws))]
    [_ #f]))

@check-module[fun/kwrest-opt1
              #:is (lambda/kwrest-opt-no-destructure? 2)]{
 fun f(~kw1 = 1, ~kw2 = 2, ~& _):
   kw1+kw2
}

@check-module[fun/kwrest-opt2
              #:is (lambda/kwrest-opt-no-destructure? 2)]{
 fun f(~kw1 = 1, ~kw2 = 2, & _, ~& _):
   kw1+kw2
}

(define (case-lambda-match? formalss aritys)
  (and (eqv? (length formalss)
             (length aritys))
       (for/and ([formals (in-list formalss)]
                 [arity (in-list aritys)])
         (define-values (rest? required-arity)
           (if (negative? arity)
               (values #t (- (add1 arity)))
               (values #f arity)))
         (let loop ([formals formals]
                    [required-arity required-arity])
           (syntax-parse formals
             [(_ . more)
              (loop #'more (sub1 required-arity))]
             [rest
              (and (eqv? required-arity 0)
                   (or rest?
                       (null? (syntax-e #'rest))))])))))

(define ((case-lambda-optimized? . aritys) mod-stx)
  (define def (extract-def mod-stx 'f))
  (syntax-parse def
    #:literals (case-lambda)
    [(~and fe
           (~parse (case-lambda
                     [formals _]
                     ...)
                   (let-body (let-named #'fe))))
     (case-lambda-match? (syntax->list #'(formals ...)) aritys)]
    [_ #f]))

@check-module[case-fun1
              #:is (case-lambda-optimized? 1)]{
 fun
 | f(a): a
}

@check-module[case-fun2
              #:is (case-lambda-optimized? 1 2)]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
}

@check-module[case-fun3
              #:is (case-lambda-optimized? 1 2 -2)]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, & bs): values(a, & bs)
}

@check-module[case-fun4
              #:is (case-lambda-optimized? 1 2 -2)]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, ...): values(a, b, ...)
}

@check-module[case-fun5
              #:is (case-lambda-optimized? 1 2 -3)]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, & cs): values(a, b, & cs)
}

@check-module[case-fun6
              #:is (case-lambda-optimized? 1 2 -3)]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, c, ...): values(a, b, c, ...)
}

@check-module[case-fun7
              #:is (case-lambda-optimized? 1 2 -4)]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, c, & ds): values(a, b, c, & ds)
}

@check-module[case-fun8
              #:is (case-lambda-optimized? 1 2 -4)]{
 fun
 | f(a): a
 | f(a, b): values(a, b)
 | f(a, b, c, d, ...): values(a, b, c, d, ...)
}

@check-module[case-fun9
              #:is (case-lambda-optimized? -3 1 -2 2 -1)]{
 fun
 | f(a, b, c, ...): values(a, b, c, ...)
 | f(a): a
 | f(a, & bs): values(a, & bs)
 | f(a, b): values(a, b)
 | f(a, ...): values(a, ...)
}

(define ((case-lambda/kw-not-optimized? . aritys) mod-stx)
  (define def (extract-def mod-stx 'f))
  (syntax-parse def
    #:literals ([#%app #%plain-app]
                let-values
                [lambda #%plain-lambda]
                case-lambda
                procedure-reduce-keyword-arity-mask
                make-keyword-procedure)
    [(~and fe
           (~parse (~or* (#%app procedure-reduce-keyword-arity-mask inner _ _ _)
                         inner)
                   (let-body #'fe))
           (~parse (#%app make-keyword-procedure
                          (case-lambda
                            [(_ _ . kw-formals) _]
                            ...)
                          (case-lambda
                            [formals _]
                            ...))
                   #'inner))
     (case-lambda-match? (syntax->list #'(kw-formals ...)) aritys)
     (case-lambda-match? (syntax->list #'(formals ...)) aritys)]
    [_ #f]))

@check-module[case-fun/kw-mixed1
              #:is (case-lambda/kw-not-optimized? 0)]{
 fun
 | f(): "no key"
 | f(~a: _): "got a"
 | f(~& _): "got something else"
}

@check-module[case-fun/kw-mixed2
              #:is (case-lambda/kw-not-optimized? 0)]{
 fun
 | f(): "no key"
 | f(~& _): "got something else"
 | f(~a: _): "got a"
}

@check-module[case-fun/kw-mixed3
              #:is (case-lambda/kw-not-optimized? 0)]{
 fun
 | f(~& _): "got something else"
 | f(): "no key"
 | f(~a: _): "got a"
}

(define (case-lambda/kwrest-not-reduced? mod-stx)
  (lambda/kwrest-not-reduced? mod-stx))

(define (case-lambda/kwrest-no-apply? mod-stx)
  (lambda/kwrest-no-apply? mod-stx))

@check-module[case-fun/kwrest1
              #:is case-lambda/kwrest-not-reduced?]{
 fun
 | f(~& kwrest): kwrest
}

@check-module[case-fun/kwrest2
              #:is case-lambda/kwrest-not-reduced?
              #:is case-lambda/kwrest-no-apply?]{
 fun
 | f(& rest): rest
 | f(~& kwrest): kwrest
}

@check-module[case-fun/kwrest3
              #:is case-lambda/kwrest-not-reduced?
              #:is case-lambda/kwrest-no-apply?]{
 fun
 | f(_, & rest): rest
 | f(~& kwrest): kwrest
}
