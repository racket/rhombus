#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "static-info.rkt"
         "function-arity-key.rkt"
         (only-in (submod "dot.rkt" for-dot-provider)
                  dot-provider)
         "dot-provider-key.rkt")

(provide define/arity
         (for-syntax set-function-dot-provider!))

(module+ dot-provider
  (provide function-dot-provider))

(define-syntax (define/arity stx)
  (syntax-parse stx
    [(_ #:name name (id . args)
        (~optional (~seq #:static-infos (si ...))
                   #:defaults ([(si 1) '()]))
        . body)
     #'(begin
         (define id
           (let ([name (lambda args . body)])
             name))
         (define-static-info-syntax id
           (#%dot-provider function-dot-provider)
           (#%function-arity #,(extract-arity #'args))
           si ...))]
    [(_ id
        (~optional (~seq #:static-infos (si ...))
                   #:defaults ([(si 1) '()]))
        (~and rhs
              ((~literal case-lambda)
               [args body ...]
               ...)))
     #'(begin
         (define id rhs)
         (define-static-info-syntax id
           (#%dot-provider function-dot-provider)
           (#%function-arity #,(apply
                                bitwise-ior
                                (map extract-arity (syntax->list #'(args ...)))))
           si ...))]
    [(_ (id . args)
        (~optional (~seq #:static-infos (si ...))
                   #:defaults ([(si 1) '()]))
        . body)
     #'(begin
         (define (id . args) . body)
         (define-static-info-syntax id
           (#%dot-provider function-dot-provider)
           (#%function-arity #,(extract-arity #'args))
           si ...))]))

(define-for-syntax (extract-arity args)
  (let loop ([args args] [mask 1] [allowed-kws '()] [req-kws '()])
    (syntax-parse args
      [() (if (null? allowed-kws)
              mask
              `(,mask #,(sort req-kws keyword<?)  #,(sort allowed-kws keyword<?)))]
      [(_:identifier . args) (loop #'args (arithmetic-shift mask 1) allowed-kws req-kws)]
      [([_:identifier _] . args) (bitwise-ior mask
                                              (loop #'args (arithmetic-shift mask 1) allowed-kws req-kws))]
      [_:identifier (loop #'() (bitwise-not (sub1 (arithmetic-shift mask 1))) allowed-kws req-kws)]
      [(kw:keyword _:identifier . args) (loop #'args (arithmetic-shift mask 1)
                                              (cons (syntax-e #'kw) allowed-kws)
                                              (cons (syntax-e #'kw) req-kws))]
      [(kw:keyword [_:identifier _] . args) (loop #'args (arithmetic-shift mask 1)
                                                  (cons (syntax-e #'kw) allowed-kws)
                                                  req-kws)])))

(define-for-syntax function-dot-provider-proc void)
(define-for-syntax (set-function-dot-provider! proc)
  (set! function-dot-provider-proc proc))

(define-syntax function-dot-provider
  (dot-provider (lambda args (apply function-dot-provider-proc args))))
