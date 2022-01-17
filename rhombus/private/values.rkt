#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "binding.rkt"
         "folder.rkt"
         "parse.rkt"
         (rename-in "equal.rkt"
                    [= rhombus=]))

(provide values
         (for-space rhombus/binding values)
         (for-space rhombus/folder values))

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

(define-folder-syntax values
  (folder-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (rhombus=)
       #:datum-literals (group op)
       [(_ (parens (group id:identifier (op rhombus=) rhs ...) ...))
        #'[begin
           ([id (rhombus-expression (group rhs ...))] ...)
           (begin)
           ()]]))))
