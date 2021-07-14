#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "transformer.rkt")
         "parse.rkt")

(provide (rename-out [rhombus-define define]))

(define-syntax rhombus-define
  (rhombus-definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block)
       [(_ id:identifier (parens arg::pattern ...) (~and rhs (block body ...)))
        #:with (arg-id ...) (generate-temporaries #'(arg ...))
        (values
         (list
          #'(define id
              (lambda (arg-id ...)
                (nested-bindings
                 (((match? . arg.bindings) (arg.filter arg-id))
                  (unless match? (argument-pattern-failure 'id arg-id 'arg)))
                 ...
                 (rhombus-expression (group rhs))))))
         null)]))))

(define-syntax nested-bindings
  (syntax-rules ()
    [(_ body) body]
    [(_ ((vars rhs) check) . tail) (let-values ([vars rhs])
                                     check
                                     (nested-bindings . tail))]))

(define (argument-pattern-failure who val pattern)
  (error who
         (string-append "argument does not match pattern\n"
                        "  argument: ~v\n"
                        "  pattern: ~s")
         val
         pattern))
