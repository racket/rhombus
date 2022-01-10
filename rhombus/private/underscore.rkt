#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "annotation-string.rkt")
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt")

(provide (rename-out [rhombus-_ _]))

(define-syntax rhombus-_
  (make-expression+binding-prefix-operator
   #'_
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (raise-syntax-error #f
                            (string-append "not allowed as an expression;\n"
                                           " only allowed as binding pattern or 'else' substitute")
                            #'form-id)]))
   ;; binding
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
                   #'nothing-bind
                   #'())]))

(define-syntax (always-succeed stx)
  (syntax-parse stx
    [(_ _ _ IF success fail)
     #'(IF #t success fail)]))

(define-syntax (nothing-bind stx)
  (syntax-parse stx
    [(_ _ _) #'(begin)]))
