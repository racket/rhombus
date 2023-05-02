#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt")
         "provide.rkt"
         "expression.rkt"
         "binding.rkt")

(provide (for-spaces (#f
                      rhombus/bind)
                     (rename-out [rhombus-_ _])))

(define-syntax rhombus-_
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (raise-syntax-error #f
                            (string-append "not allowed as an expression;\n"
                                           " only allowed as binding pattern or 'else' substitute")
                            #'form-id)]))))

(define-binding-syntax rhombus-_
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'ignored-info
                              #'#f)
                #'tail)]))))

(define-syntax (ignored-info stx)
  (syntax-parse stx
    [(_ static-infos _)
     (binding-info annotation-any-string
                   #'ignored
                   #'static-infos
                   #'()
                   #'always-succeed
                   #'nothing-commit
                   #'nothing-bind
                   #'())]))

(define-syntax (nothing-commit stx)
  (syntax-parse stx
    [(_ _ _) #'(begin)]))

(define-syntax (nothing-bind stx)
  (syntax-parse stx
    [(_ _ _) #'(begin)]))
