#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "binding.rkt"
         "reducer.rkt"
         "parse.rkt"
         (rename-in "equal.rkt"
                    [= rhombus=]))

(provide values
         (for-space rhombus/bind values)
         (for-space rhombus/reducer values))

(define-binding-syntax values
  (binding-prefix-operator
   #'values
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            (string-append "not allowed as a pattern (except as a non-nested"
                                           " pattern by forms that specifically recognize it")
                            #'head)]))))

(define-reducer-syntax values
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (rhombus=)
       #:datum-literals (group op)
       [(_ (parens (group id:identifier (op rhombus=) rhs ...) ...))
        #'[begin
           ([id (rhombus-expression (group rhs ...))] ...)
           (begin)
           ()]]))))
