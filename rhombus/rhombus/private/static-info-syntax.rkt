#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/ref-parse
                     "tail.rkt")
         "definition.rkt"
         "name-root.rkt"
         "quasiquote.rkt"
         "static-info.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         (for-syntax "name-root.rkt")
         (for-syntax "parse.rkt"))

(provide static_info
         (for-syntax static_info_ct))

(module+ for-static-info
  (provide (for-syntax (rename-out [pack pack-static-info]))))

(define-syntax static_info
  (simple-name-root macro))

(begin-for-syntax
  (define-syntax static_info_ct
    (simple-name-root pack
                      wrap)))

(define-syntax macro
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (op block)
        #:literals (?)
        [(_ (op ?) ref::reference (block body ...))
         #`((define-syntax #,(in-static-info-space #'ref.name)
              (convert-static-info 'ref.name (rhombus-block body ...))))]))))

(define-for-syntax (convert-static-info who stx)
  (unless (syntax? stx)
    (raise-result-error who "syntax?" stx))
  (static-info (syntax->list (pack stx))))

(define-for-syntax (pack v)
  (datum->syntax
   #f
   (map (lambda (p)
          (syntax-parse p
            #:datum-literals (group parens)
            [(parens (group key) (group val))
             #'(key val)]))
        (syntax->list (unpack-tail v pack)))))

(define-for-syntax (wrap form info)
  #`(parsed #,(wrap-static-info* (wrap-expression form)
                                 (pack info))))
