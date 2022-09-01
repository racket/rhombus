#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "definition.rkt"
         (submod "dot.rkt" for-dynamic-static)
         (submod "implicit.rkt" for-dynamic-static))

(provide dynamic
         use_dynamic
         use_static)

(define (dynamic v) v)

(define-syntaxes (use_dynamic use_static)
  (let ([mk (lambda (more-static?)
              (definition-transformer
                (lambda (stx)
                  (syntax-parse stx
                    [(form-id)
                     #`((define-syntax #,(datum->syntax #'form-id '|.|) (make-|.| #,more-static?))
                        (define-syntax #,(datum->syntax #'form-id '#%ref) (make-#%ref #,more-static?)))]))))])
    (values (mk #f)
            (mk #t))))
