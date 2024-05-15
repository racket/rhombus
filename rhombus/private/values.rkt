#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "provide.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "reducer.rkt"
         "parse.rkt"
         "static-info.rkt"
         "function-arity-key.rkt"
         "values-key.rkt"
         (submod "define-arity.rkt" for-info)
         "indirect-static-info-key.rkt"
         "parens.rkt"
         "op-literal.rkt"
         "var-decl.rkt")

(provide (for-spaces (#f
                      rhombus/bind
                      rhombus/annot
                      rhombus/reducer
                      rhombus/statinfo)
                     values)
         (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [call-with-values call_with_values])))

(define-binding-syntax values
  (binding-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            (string-append "not allowed as a pattern (except as a non-nested"
                                           " pattern by forms that specifically recognize it)")
                            #'head)]))))

(define-annotation-syntax values
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            (string-append "not allowed as an annotation (except as a non-nested"
                                           " annotation by forms that specifically recognize it)")
                            #'head)]))))

(begin-for-syntax
  (define-splicing-syntax-class :accum
    #:description "accumulator with optional annotation"
    #:attributes (id e check make-check static-infos)
    (pattern d::var-decl
      #:with ((~or* (~and _::_-bind
                          (~parse id (car (generate-temporaries '(wildcard)))))
                    id:identifier)
              (~optional c::inline-annotation))
      #'(d.bind ...)
      #:with e #'d.default
      #:do [(define-values (check make-check)
              (if (cond
                    [(attribute c.converter) => syntax-e]
                    [else #f])
                  (values (car (generate-temporaries '(check)))
                          #'(lambda (who)
                              (lambda (val)
                                (c.converter
                                 val who
                                 (lambda (val who)
                                   (raise-annotation-failure who val 'c.annotation-str))))))
                  (values #f #f)))]
      #:attr check check
      #:attr make-check make-check
      #:with static-infos #'(~? c.static-infos ()))))

(define-reducer-syntax values
  (reducer-transformer
   (lambda (stx)
     (syntax-parse (respan stx)
       #:datum-literals (group)
       [(form-id (_::parens (group accum::accum) ...) . tail)
        (values
         (reducer/no-break #'build-values-check-bind
                           #'([accum.id (~? (let ([accum.id accum.e])
                                              (accum.check accum.id)
                                              accum.id)
                                            accum.e)]
                              ...)
                           #:pre-clause #'build-values-static-info
                           #'build-values-check
                           #'((#%values (accum.static-infos
                                         ...)))
                           #'([accum.id accum.static-infos
                                        (~? [accum.check (accum.make-check 'form-id)])]
                              ...))
         #'tail)]))))

(define-syntax (build-values-check-bind stx)
  (syntax-parse stx
    [(_ ([_ _ (~optional (~and check-bind [_ _]))] ...) e)
     #'(let ((~? check-bind) ...)
         e)]))

(define-syntax (build-values-static-info stx)
  (syntax-parse stx
    [(_ ([id si (~optional [_ _])] ...))
     #'(begin
         (define-static-info-syntax/maybe id . si)
         ...)]))

(define-syntax (build-values-check stx)
  (syntax-parse stx
    [(_ ([id _ (~optional [check _])] ...) e)
     #'(let-values ([(id ...) e])
         (~? (check id))
         ...
         (values id ...))]))

(define-static-info-syntax values
  (#%function-arity -1)
  (#%indirect-static-info indirect-function-static-info))

(define-static-info-syntax call-with-values
  (#%function-arity 4)
  (#%indirect-static-info indirect-function-static-info))
