#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt")
         "name-root.rkt"
         (for-syntax "name-root.rkt")
         (submod "dot.rkt" for-dot-provider)
         "space.rkt"
         "macro-macro.rkt"
         "parse.rkt"
         "wrap-expression.rkt")

(provide dot)

(define-name-root dot
  #:root (space-syntax rhombus/dot)
  #:fields
  (macro
   macro_more_static
   only))

(define-name-root only
  #:fields
  ([macro macro-only]
   [macro_more_static macro_more_static-only]))

(define-for-syntax provider_key #'#%dot-provider)

(define-identifier-syntax-definition-transformer+only macro macro-only
  rhombus/dot
  #'make-dot-provider-transformer)

(define-identifier-syntax-definition-transformer+only macro_more_static macro_more_static-only
  rhombus/dot
  #'make-dot-provider-more-static-transformer)

(define-for-syntax (make-dot-provider-transformer proc)
  (dot-provider
   (lambda (left dot right)
     (define e (proc (pack-tail #`((parsed #,left) #,dot #,right)) dot))
     (and e
          (wrap-expression e)))))

(define-for-syntax (make-dot-provider-more-static-transformer proc)
  (dot-provider-more-static
   (lambda (left dot right tail static? success-k fail-k)
     (call-with-values
      (lambda () (proc (pack-tail #`((parsed #,left) #,dot #,right . #,tail)) dot))
      (case-lambda
        [(false)
         (when false
           (error (proc-name proc)
                  (format (string-append "expected a single-value result as #false\n"
                                         "  received: ~.v")
                          false)))
         (fail-k)]
        [(e tail)
         (if e
             (success-k (wrap-expression e) (unpack-tail tail proc #f))
             (fail-k))])))))
