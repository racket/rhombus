#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "definition.rkt"
         "expression.rkt"
         "repetition.rkt"
         (submod "dot.rkt" for-dynamic-static)
         (submod "implicit.rkt" for-dynamic-static))

(provide dynamic
         use_dynamic
         use_static)
         
(define dynamic (lambda (v) v))

(begin-for-syntax
  (define-values (use_dynamic use_static)
    (let ([mk (lambda (more-static?)
                (definition-transformer
                  (lambda (stx)
                    (syntax-parse stx
                      [(form-id)
                       #`(#,@(build-definitions #'form-id '|.| (if more-static? #'static-|.| #'|.|))
                          #,@(build-definitions #'form-id '#%ref (if more-static? #'static-#%ref #'#%ref))
                          #,@(build-definitions #'form-id '#%call (if more-static? #'static-#%call #'#%call)))]))))])
      (values (mk #f)
              (mk #t)))))

(define-syntax use_dynamic use_dynamic)
(define-syntax use_static use_static)

(define-for-syntax (build-definitions ctx sym id)
  (define sym-id (datum->syntax ctx sym))
  #`((define-syntax #,sym-id
       (make-rename-transformer (quote-syntax #,id)))
     (define-syntax #,(in-repetition-space sym-id)
       (make-rename-transformer (quote-syntax #,(in-repetition-space id))))))
