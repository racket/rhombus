#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         rhombus/private/parse
         (prefix-in doc: scribble/doclang2)
         scribble/base)

(provide (rename-out [module-begin #%module-begin])
         (all-from-out scribble/base))
(define-syntax-rule (rhombus-out)
  (begin
    (require (except-in rhombus #%module-begin))
    (provide (all-from-out rhombus))))
(rhombus-out)
              

(module reader syntax/module-reader
  #:language 'scribble/rhombus
  #:read (lambda (in) (list (syntax->datum (parse-all in #:test-mode? #t))))
  #:read-syntax (lambda (src in) (list (parse-all in #:text-mode? #t #:source src)))
  #:info (lambda (key default make-default)
           (case key
             [(color-lexer)
              (dynamic-require 'shrubbery/syntax-color
                               'shrubbery-text-mode-lexer)]
             [else (get-info-proc key default make-default)]))
  #:whole-body-readers? #t
  (require shrubbery/parse
           (only-in (submod shrubbery reader)
                    get-info-proc)))

(define-syntax (module-begin stx)
  (syntax-parse stx
    #:datum-literals (brackets)
    [(_ (brackets g ...))
     #'(doc:#%module-begin (rhombus-expression g)
                           ...)]))
