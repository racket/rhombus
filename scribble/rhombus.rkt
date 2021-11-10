#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         rhombus/private/parse
         (prefix-in doc: scribble/doclang2)
         (rename-in scribble/base
                    [verbatim base:verbatim])
         scribble/private/manual-defaults
         "private/rhombus.rkt")

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scribble/base)
                     base:verbatim)
         verbatim
         (all-from-out "private/rhombus.rkt"))
(define-syntax-rule (rhombus-out)
  (begin
    (require (except-in rhombus #%module-begin))
    (provide (all-from-out rhombus))))
(rhombus-out)

(module reader syntax/module-reader
  #:language 'scribble/rhombus
  #:read read-proc
  #:read-syntax read-syntax-proc
  #:info get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (only-in (submod shrubbery reader)
                    [get-info-proc shrubbery:get-info-proc]))
  (provide read-proc
           read-syntax-proc
           get-info-proc)
  (define (read-proc in)
    (list (syntax->datum (parse-all in #:test-mode? #t))))
  (define (read-syntax-proc src in)
    (list (parse-all in #:text-mode? #t #:source src)))
  (define (get-info-proc key default make-default)
    (case key
      [(color-lexer)
       (dynamic-require 'shrubbery/syntax-color
                        'shrubbery-text-mode-lexer)]
      [else (shrubbery:get-info-proc key default make-default)])))

(define-syntax (module-begin stx)
  (syntax-parse stx
    #:datum-literals (brackets)
    [(_ (brackets g ...))
     #'(doc:#%module-begin
        #:post-process post-process
        (rhombus-expression g)
        ...)]))

;; ----------------------------------------

(define (verbatim #:indent [indent 0] l)
  (base:verbatim #:indent indent (car l)))
