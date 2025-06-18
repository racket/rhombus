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
         "realm.rkt"
         "is-static.rkt")

(provide (for-syntax compose-dot-providers
                     wrap-class-dot-provider-transformers
                     wrap-class-dot-via-class))

(define-for-syntax (compose-dot-providers . dps)
  (let loop ([dps dps])
    (cond
      [(null? dps) (lambda (form1 dot field-id tail more-static? repetition? success failure)
                     (failure))]
      [else
       (define (convert v)
         (if (procedure? v)
             v
             (syntax-local-value v)))
       (let ([main (convert (car dps))]
             [next (loop (cdr dps))])
         (lambda (form1 dot field-id tail more-static? repetition? success failure)
           (main form1 dot field-id tail more-static? repetition?
                 success
                 (lambda ()
                   (next form1 dot field-id tail more-static? repetition? success failure)))))])))

(define-for-syntax (wrap-class-dot-provider-transformers ids-stx)
  (apply
   compose-dot-providers
   (map
    (lambda (id)
      (define proc (syntax-local-value id))
      (wrap-dot-provider-transformer
       (lambda (packed-form dot static? repetition? packed-tail)
         (syntax-parse (unpack-tail packed-form proc #f)
           [(lhs dot-op name . _)
            (proc packed-form dot (no-srcloc #`(#,group-tag lhs dot-op name)) static? repetition? packed-tail)]))
       '(#:is_repet)))
    (syntax->list ids-stx))))

(define-for-syntax (wrap-class-dot-via-class proc name pred dot-provider)
  (make-expression-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (packed-tail self-stx)
     (syntax-parse (unpack-tail packed-tail proc #f)
       [((~and p (_::parens g)) . tail)
        #:with e::expression #'g
        (define new-g (pack-tail
                       #`((parsed #:rhombus/expr
                                  #,(wrap-static-info
                                     #`(let ([o #,(discard-static-infos #'e.parsed)])
                                         (check-instance '#,name #,pred o)
                                         o)
                                     #'#%dot-provider
                                     dot-provider))
                          (op |.|)
                          #,name)))
        (define call-g (no-srcloc #`(#,group-tag #,self-stx p)))
        (define is-static? (is-static-context? self-stx))
        (define repetition? #false)
        (define orig-tail (pack-tail #'tail))
        (call-with-values
         (lambda ()
           (proc new-g name call-g is-static? repetition? orig-tail))
         ;; different default tail:
         (case-lambda
           [(e) (values e orig-tail)]
           [(e tail) (values e tail)]))]))))

(define (check-instance name pred v)
  (unless (pred v)
    (raise-arguments-error* name rhombus-realm
                            "not an instance for dot syntax"
                            "value" v)))
