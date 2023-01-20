#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-root
                     "srcloc.rkt"
                     "introducer.rkt")
         "name-root-space.rkt")

(provide define-name-root)

(define-syntax (define-name-root stx)
  (syntax-parse stx
    [(_ id (~alt (~once (~seq #:fields (content ...)))
                 (~optional (~seq #:extends extends)
                            #:defaults ([extends #'#f]))
                 (~optional (~seq #:orig-id orig-id)
                            #:defaults ([orig-id #'#f])))
        ...)
     #:with (norm-content ...) (for/list ([c (in-list (syntax->list #'(content ...)))])
                                 (syntax-parse c
                                   [_:identifier #`[#,c #,c]]
                                   [(_:identifier _:identifier) c]))
     #:with space-id (in-name-root-space #'id)
     #:with the-orig-id (if (syntax-e #'orig-id)
                            #'orig-id
                            #'space-id)
     #'(begin
         ;; portal syntax with this shape is recognized by "name-root-ref.rkt"
         (#%require (portal space-id (map the-orig-id extends norm-content ...))))]))
