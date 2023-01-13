#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "realm.rkt"
                     (submod "class-meta.rkt" for-static-info))
         "syntax-binding.rkt"
         "name-root.rkt"
         "syntax.rkt")

(provide syntax_binding)

(define-simple-name-root syntax_binding
  macro
  only)

(define-name-root only
  #:fields
  ([macro macro-only]))

(define-operator-definition-transformer+only macro macro-only
  'macro
  rhombus/syntax_binding
  #'make-syntax-binding-prefix-operator
  #'make-syntax-binding-infix-operator
  #'syntax-binding-prefix+infix-operator)

(define-for-syntax (make-syntax-binding-prefix-operator name prec protocol proc)
  (syntax-binding-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 stx)
         (finish (lambda ()
                   (syntax-parse stx
                     [(head . tail) (proc form1 (pack-tail #'tail) #'head)]))
                 proc))
       (lambda (stx)
         (finish (lambda ()
                   (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head)]))
                 proc)))))

(define-for-syntax (make-syntax-binding-infix-operator name prec protocol proc assc)
  (syntax-binding-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (finish (lambda ()
                   (syntax-parse stx
                     [(head . tail) (proc form1 form2 (pack-tail #'tail) #'head)]))
                 proc))
       (lambda (form1 stx)
         (finish (lambda ()
                   (syntax-parse stx
                     [(head . tail) (proc form1 (pack-tail #'tail) #'head)]))
                 proc)))
   assc))

(define-for-syntax (finish thunk proc)
  (define-values (binds tail)
    (call-with-values
     thunk
     (case-lambda
       [(binds tail) (values binds (unpack-tail tail proc #f))]
       [(binds) (values binds #'())])))
  (unless (syntax? binds)
    (raise-result-error* (proc-name proc) rhombus-realm "Syntax" binds))
  (values (syntax-parse (unpack-group binds proc binds)
            [esc::syntax-binding #'esc.parsed])
          tail))
