#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "op.rkt")
         (prefix-in core: "core-op.rkt"))

(provide |.|)

(define-syntax |.|
  (rhombus-infix-operator-transformer
   (quote-syntax |.|)
   '((default . stronger))
   'left
   (lambda (form1 tail)
     (syntax-parse tail
       [(dot field:identifier . tail)
        (values (datum->syntax (quote-syntax here)
                               (list #'field-ref form1 #''field)
                               (span-srcloc form1 #'field)
                               #'dot)
                #'tail)]
       [(dot other . tail)
        (raise-syntax-error #f
                            "expected an identifier for a field name, but found something else"
                            #'dot
                            #f
                            (list #'other))]
       [(dot)
        (raise-syntax-error #f
                            "expected an identifier for a field name"
                            #'dot)]))))

(define (field-ref v name)
  (error 'field-ref "don't yet know how to find field: ~s" name))
