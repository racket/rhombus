#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "provide.rkt"
         "binding.rkt"
         "reducer.rkt"
         "parse.rkt"
         "static-info.rkt"
         "function-arity-key.rkt"
         (submod "equal.rkt" for-parse)
         (submod "define-arity.rkt" for-info)
         "indirect-static-info-key.rkt")

(provide (for-spaces (#f
                      rhombus/bind
                      rhombus/reducer
                      rhombus/statinfo)
                     values)
         (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [call-with-values call_with_values])))

(define-binding-syntax values
  (binding-prefix-operator
   #'values
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            (string-append "not allowed as a pattern (except as a non-nested"
                                           " pattern by forms that specifically recognize it)")
                            #'head)]))))

(define-reducer-syntax values
  (reducer-transformer
   (lambda (stx)
     (syntax-parse (respan stx)
       #:datum-literals (group op)
       [(_ (parens (group id:identifier _::equal rhs ...) ...) . tail)
        #:with (e::expression ...) #'((group rhs ...) ...)
        #:with (e2 ...) (map rhombus-local-expand (syntax->list #'(e.parsed ...)))
        #:with (si ...) (map extract-static-infos (syntax->list #'(e2 ...)))
        (values
         (reducer/no-break #'build-return
                           #'([id e2] ...)
                           #'build-static-info
                           #'()
                           #'([id si] ...))
         #'tail)]))))

(define-syntax (build-return stx)
  (syntax-parse stx
    [(_ _ e) #'e]))

(define-syntax (build-static-info stx)
  (syntax-parse stx
    [(_ ([id si] ...) e)
     #'(let ()
         (define-static-info-syntax/maybe id . si)
         ...
         e)]))

(define-static-info-syntax values
  (#%function-arity -1)
  (#%indirect-static-info indirect-function-static-info))

(define-static-info-syntax call-with-values
  (#%function-arity 4)
  (#%indirect-static-info indirect-function-static-info))
