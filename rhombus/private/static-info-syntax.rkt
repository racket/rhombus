#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/name-parse
                     "tail.rkt"
                     "static-info-pack.rkt")
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

(define-syntax static_info
  (simple-name-root macro))

(begin-for-syntax
  (define-syntax static_info_ct
    (simple-name-root pack
                      unpack
                      wrap)))

(define-syntax macro
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (op block)
        #:literals (|'|)
        [(_ (op |'|) name::name ((~and body-tag block) body ...))
         #`((define-syntax #,(in-static-info-space #'name.name)
              (convert-static-info 'name.name (rhombus-body-at body-tag body ...))))]))))

(define-for-syntax (convert-static-info who stx)
  (unless (syntax? stx)
    (raise-result-error who "syntax?" stx))
  (static-info (syntax->list (pack stx))))

(define-for-syntax (pack v)
  (pack-static-infos v 'static_info_ct.pack))

(define-for-syntax (unpack v)
  (unpack-static-infos v))

(define-for-syntax (wrap form info)
  #`(parsed #,(wrap-static-info* (wrap-expression form)
                                 (pack info))))
