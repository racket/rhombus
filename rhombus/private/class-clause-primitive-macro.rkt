#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     "name-path-op.rkt"
                     "class-parse.rkt"
                     "macro-expr.rkt")
         "provide.rkt"
         "class-clause.rkt"
         "class-clause-tag.rkt"
         "interface-clause.rkt"
         "veneer-clause.rkt"
         "parens.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt")

(provide (for-space rhombus/class_clause
                    binding)
         (for-spaces (rhombus/class_clause
                      rhombus/interface_clause
                      rhombus/veneer_clause)
                     expression
                     annotation))

;; see also "class-clause-primitive-meta-macro.rkt", which provides
;; forms that are exported only by `rhombus/meta`, because they
;; require meta-time binding to be useful (unlike `expression`,
;; `binding`, and `annotation`, which have a `macro`-like form that
;; works even without meta imports).

(define-for-syntax (parse-multiple-names stx)
  (define lines
    (syntax-parse stx
      [(_ (tag::block (group form ...) ...))
       (syntax->list #'((form ...) ...))]
      [(_ form ...)
       (list #'(form ...))]))
  (apply append
         (for/list ([line (in-list lines)])
           (let loop ([line line])
             (syntax-parse line
               [() null]
               [(~var id (:hier-name-seq in-name-root-space in-class-desc-space name-path-op name-root-ref))
                (cons #'id.name (loop #'id.tail))])))))

(define-for-syntax (make-macro-clause-transformer
                    key
                    #:clause-transformer [clause-transformer class-clause-transformer])
  (clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-name (~and (_::quotes . _)
                         pattern)
                   (~and (_::block . _)
                         template-block))
        (wrap-class-clause #`(#,key (block (named-macro macro-expression #,stx pattern template-block))))]
       [(form-name (~and rhs (_::alts
                              (_::block (group (_::quotes . _)
                                               (_::block . _)))
                              ...)))
        (wrap-class-clause #`(#,key (block (named-macro macro-expression #,stx rhs))))]
       [(form-name (~and (_::block . _)
                         a-block))
        (wrap-class-clause #`(#,key a-block))]))))

(define-class-clause-syntax binding
  (make-macro-clause-transformer #'#:binding))

(define-class-clause-syntax annotation
  (make-macro-clause-transformer #'#:annotation))

(define-interface-clause-syntax annotation
  (make-macro-clause-transformer #'#:annotation
                                 #:clause-transformer interface-clause-transformer))

(define-veneer-clause-syntax annotation
  (make-macro-clause-transformer #'#:annotation
                                 #:clause-transformer veneer-clause-transformer))

(define-class-clause-syntax expression
  (make-macro-clause-transformer #'#:expression))

(define-interface-clause-syntax expression
  (make-macro-clause-transformer #'#:expression
                                 #:clause-transformer interface-clause-transformer))

(define-veneer-clause-syntax expression
  (make-macro-clause-transformer #'#:expression
                                 #:clause-transformer veneer-clause-transformer))
