#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "pack.rkt"
                     "tag.rkt"
                     "srcloc.rkt")
         (submod "dot-macro.rkt" for-compose)
         (submod "expr-macro.rkt" for-define)
         "static-info.rkt"
         "dot-provider-key.rkt"
         "parse.rkt"
         "parens.rkt"
         "realm.rkt")

(provide (for-syntax compose-dot-providers
                     wrap-class-dot-provider-transformers
                     wrap-class-dot-via-class))

(define-for-syntax (compose-dot-providers . dps)
  (let loop ([dps dps])
    (cond
      [(null? dps) (lambda (form1 dot field-id tail more-static? success failure)
                     (failure))]
      [else
       (define (convert v)
         (if (procedure? v)
             v
             (syntax-local-value v)))
       (let ([main (convert (car dps))]
             [next (loop (cdr dps))])
         (lambda (form1 dot field-id tail more-static? success failure)
           (main form1 dot field-id tail more-static?
                 success
                 (lambda ()
                   (next form1 dot field-id tail more-static? success failure)))))])))

(define-for-syntax (wrap-class-dot-provider-transformers ids-stx)
  (apply
   compose-dot-providers
   (map
    (lambda (id)
      (define proc (syntax-local-value id))
      (wrap-dot-provider-transformer
       (lambda (packed-form dot static? packed-tail)
         (syntax-parse packed-form
           [(_ (_ lhs dot-op name . _))
            (proc packed-form dot (no-srcloc #`(#,group-tag lhs dot-op name)) static? packed-tail)]))))
    (syntax->list ids-stx))))

(define-for-syntax (wrap-class-dot-via-class proc name pred dot-provider)
  (make-expression-prefix-operator
   #'ignored
   '((default . stronger))
   'macro
   (lambda (packed-tail self)
     (syntax-parse packed-tail
       [(_ (_ (~and p (_::parens g)) . tail))
        #:with e::expression #'g
        (define new-g #`((parsed #:rhombus/expr
                                 #,(wrap-static-info
                                    #`(let ([o e.parsed])
                                        (check-instance '#,name #,pred o)
                                        o)
                                    #'#%dot-provider
                                    dot-provider))
                         |.|
                         #,name))
        (define orig-tail (pack-tail #'tail))
        (call-with-values
         (lambda ()
           (proc (pack-tail new-g) name new-g #f orig-tail))
         ;; different default tail:
         (case-lambda
           [(e) (values e orig-tail)]
           [args (apply values args)]))]))))

(define (check-instance name pred v)
  (unless (pred v)
    (raise-arguments-error* name rhombus-realm
                            "not an instance for dot syntax"
                            "value" v)))
