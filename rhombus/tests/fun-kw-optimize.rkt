#lang racket/base

(require syntax/modread
         syntax/parse/pre)
(module+ test
  (require rackunit))

(define fun-kw #<<```
#lang rhombus

fun f(x, ~b):
  b

fun g():
  f(1, ~b: 2)
```
  )

(define stx
  (expand
   (check-module-form
    (parameterize ([port-count-lines-enabled #true])
      (with-module-reading-parameterization
        (λ () (read-syntax 'fun-kw (open-input-string fun-kw)))))
    'fun-kw
    "fun-kw")))

(define (let-body stx)
  (syntax-parse stx
    #:datum-literals (let-values)
    [(let-values _ body:expr) (let-body #'body)]
    [_ stx]))

(module+ test
  (syntax-parse stx
    #:datum-literals (module #%module-begin define-values g lambda let-values)
    [(module _ _
       (#%module-begin
        {~alt {~once (define-values (g) ge:expr)}
              _}
        ...))
     #:with (lambda () gb) (let-body #'ge)
     (check-true (syntax-parse (let-body #'gb)
                   #:datum-literals (if #%app variable-reference-constant?
                                        #%variable-reference)
                   [(if (#%app variable-reference-constant? (#%variable-reference _))
                        (#%app _ _ _)
                        _)
                    #true]
                   [_
                    #false]))]))
