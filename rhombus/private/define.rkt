#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "transformer.rkt")
         "binding.rkt"
         "parse.rkt")

(provide (rename-out [rhombus-define define]))

(define-syntax rhombus-define
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group block)
       [(form-id:identifier id:identifier (parens arg::binding ...) (~and rhs (block body ...)))
        #:with (arg-id ...) (generate-temporaries #'(arg ...))
        (values
         (list
          #'(define id
              (lambda (arg-id ...)
                (nested-bindings
                 form-id
                 (begin)
                 (arg-id arg.expanded)
                 ...
                 (rhombus-expression (group rhs))))))
         null)]))))

(define-syntax (nested-bindings stx)
  (syntax-parse stx
    [(_ who post-defn body) #'(let () post-defn body)]
    [(_ who post-defn (arg-id arg::binding-form) . tail)
     #'(let-values ([(match? . arg.var-ids) (arg.check-proc-expr arg-id)])
         (if match?
             (nested-bindings
              who
              (begin post-defn arg.post-defn)
              . tail)
             (argument-binding-failure 'who arg-id 'arg)))]))

(define (argument-binding-failure who val binding)
  (error who
         (string-append "argument does not match binding pattern\n"
                        "  argument: ~v\n"
                        "  binding: ~s")
         val
         binding))
