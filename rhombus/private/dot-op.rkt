#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "op.rkt")
         (prefix-in core: "core-op.rkt"))

(provide |.|)

(begin-for-syntax
  (struct rhombus-infix-operator-transformer (infix)
    #:property prop:rhombus-infix-operator-transformer
    (lambda (self) (rhombus-infix-operator-transformer-infix self))))

(define-syntax |.|
  (rhombus-infix-operator-transformer
   (rhombus-infix-operator
    (quote-syntax |.|)
    (list)
    (list)
    (list #'core:+ #'core:- #'core:* #'core:/
          #'core:< #'core:<= #'core:== #'core:>= #'core:>
          #'core:&& #'core:\|\| #'core:!)
    'left
    (lambda (form1 tail stx)
      (syntax-parse tail
        [(field:identifier . tail)
         (values (datum->syntax (quote-syntax here)
                                (list #'field-ref form1 #''field)
                                (span-srcloc form1 #'field)
                                stx)
                 #'tail)]
        [(other . tail)
         (raise-syntax-error #f
                             "expected an identifier for a field name, but found something else"
                             stx
                             #f
                             (list #'other))]
        [()
         (raise-syntax-error #f
                             "expected an identifier for a field name"
                             stx)])))))

(define (field-ref v name)
  (error 'field-ref "don't yet know how to find field: ~s" name))
