#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "macro-result.rkt"
                     (submod "class-meta.rkt" for-static-info))
         "space-provide.rkt"
         "unquote-binding.rkt"
         "name-root.rkt"
         "macro-macro.rkt")

(define+provide-space unquote_bind rhombus/unquote_bind
  #:fields
  (macro))

(define-operator-definition-transformer macro
  'macro
  rhombus/unquote_bind
  #'make-unquote-binding-prefix-operator
  #'make-unquote-binding-infix-operator
  #'unquote-binding-prefix+infix-operator)

(define-for-syntax (make-unquote-binding-prefix-operator name prec protocol proc)
  (unquote-binding-prefix-operator
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

(define-for-syntax (make-unquote-binding-infix-operator name prec protocol proc assc)
  (unquote-binding-prefix-operator
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
    (raise-bad-macro-result (proc-name proc) "unquote binding" binds))
  (values (syntax-parse (unpack-group binds proc binds)
            [esc::unquote-binding #'esc.parsed])
          tail))
