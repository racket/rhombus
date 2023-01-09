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
                     #`((define-syntax #,(datum->syntax #'form-id '|.|)
                          (make-rename-transformer (quote-syntax #,(if more-static? #'static-|.| #'|.|))))
                        (define-syntax #,(datum->syntax #'form-id '#%ref)
                          (make-rename-transformer (quote-syntax #,(if more-static? #'static-#%ref #'#%ref))))
                        (define-syntax #,(datum->syntax #'form-id '#%call)
                          (make-rename-transformer (quote-syntax #,(if more-static? #'static-#%call #'#%call)))))]))))])
    (values (mk #f)
            (mk #t))))
