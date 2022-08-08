#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/name-root
                     "srcloc.rkt"
                     "introducer.rkt")
         "dot.rkt")

(provide define-simple-name-root
         define-name-root)

(define-syntax-rule (define-simple-name-root id content ...)
  ;; portal syntax with this shape is recognized by "name-root-ref.rkt"
  (#%require (portal id (map [content content] ...))))

(define-syntax (define-name-root stx)
  (syntax-parse stx
    [(_ id (~alt (~once (~seq #:fields (content ...)))
                 (~optional (~seq #:root root-rhs)
                            #:defaults ([root-rhs #'#f]))
                 (~optional (~seq #:space space)
                            #:defaults ([space #'#f])))
        ...)
     #:do [(define in-space
             (let ([space (syntax-e #'space)])
               (if space
                   (make-interned-syntax-introducer/add space)
                   (lambda (id) id))))]
     #:with root-id (in-space (car (generate-temporaries #'(id))))
     #:with space-id (in-space #'id)
     #:with (root-def ...) (if (syntax-e #'root-rhs)
                               #'[(define-syntax root-id root-rhs)]
                               #'[])
     #:with (root-spec ...) (if (syntax-e #'root-rhs)
                                #'([#f root-id])
                                #'())
     #:with (norm-content ...) (for/list ([c (in-list (syntax->list #'(content ...)))])
                                 (syntax-parse c
                                   [_:identifier #`[#,c #,c]]
                                   [(_:identifier _:identifier) c]))
     #'(begin
         root-def ...
         (#%require (portal space-id (map norm-content ... root-spec ...))))]))
