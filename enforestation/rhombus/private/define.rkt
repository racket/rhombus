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
       [(_ id:identifier (parens arg::binding ...) (~and rhs (block body ...)))
        #:with (arg-id ...) (generate-temporaries #'(arg ...))
        (values
         (list
          #'(define id
              (lambda (arg-id ...)
                (nested-bindings
                 ((match? . arg.variable-ids)
                  (arg.matcher-form arg-id)
                  (unless match? (argument-binding-failure 'id arg-id 'arg))
                  arg.syntax-ids
                  arg.syntax-form)
                 ...
                 (rhombus-expression (group rhs))))))
         null)]))))

(define-syntax nested-bindings
  (syntax-rules ()
    [(_ body) body]
    [(_ (vars var-rhs check stxes stx-rhs) . tail)
     (let-values ([vars var-rhs])
       check
       (letrec-syntaxes ([stxes stx-rhs])
         (nested-bindings . tail)))]))

(define (argument-binding-failure who val binding)
  (error who
         (string-append "argument does not match binding pattern\n"
                        "  argument: ~v\n"
                        "  binding: ~s")
         val
         binding))
