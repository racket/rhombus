#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "realm.rkt")
         "name-root-ref.rkt")

(provide define-folder-syntax)

(module+ for-class
  (provide (for-syntax in-folder-space)))

(begin-for-syntax
  (provide (property-out folder-transformer)
           :folder
           :folder-form)

  (property folder-transformer transformer)

  (define-syntax-class :folder-form
    (pattern [wrapper
              ([id:identifier init-expr] ...)
              body-wrapper
              static-infos]
             #:attr binds #'([id init-expr] ...)))

  (define (check-folder-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::folder-form form]
      [_ (raise-result-error (proc-name proc) rhombus-realm "Folder_Syntax" form)]))

  (define in-folder-space (make-interned-syntax-introducer/add 'rhombus/folder))
  
  (define-transform
    #:syntax-class :folder
    #:desc "folder"
    #:in-space in-folder-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:transformer-ref folder-transformer-ref
    #:check-result check-folder-result))

(define-syntax (define-folder-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-folder-space #'id)
         rhs)]))
