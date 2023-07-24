#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "definition.rkt"
         (submod "is-static.rkt" for-dynamic-static))

(provide dynamic
         #%dynamism
         (for-space rhombus/defn
                    use_dynamic
                    use_static))
         
(define dynamic (lambda (v) v))

(define-syntax #%dynamism (make-rename-transformer #'#%dynamic))

(begin-for-syntax
  (define-values (use_dynamic use_static)
    (let ([mk (lambda (more-static?)
                (definition-transformer
                  (lambda (stx)
                    (syntax-parse stx
                      [(form-id)
                       #`(#,@(build-definitions #'form-id (if more-static? #'#%static #'#%dynamic)))]))))])
      (values (mk #f)
              (mk #t)))))

(define-defn-syntax use_dynamic use_dynamic)
(define-defn-syntax use_static use_static)

(define-for-syntax (build-definitions ctx id)
  (define sym-id (datum->syntax ctx '#%dynamism))
  #`((define-syntax #,sym-id (make-rename-transformer (quote-syntax #,id)))))
