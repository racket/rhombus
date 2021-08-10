#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "bind-input-key.rkt")

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
        (values (binding-form #'ignored
                              #'() ; static-infos
                              #'() ; bind-ids
                              #'always-succeed
                              #'nothing-bind
                              #'#f)
                #'tail)]))))

(define-syntax (always-succeed stx)
  (syntax-parse stx
    [(_ _ _ IF success fail)
     #'(IF #t success fail)]))

(define-syntax (nothing-bind stx)
  (syntax-parse stx
    [(_ _ _) #'(begin)]))
