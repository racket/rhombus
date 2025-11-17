#lang racket/base
(require syntax/parse/pre
         (for-template racket/unsafe/undefined
                       "parens.rkt")
         "dotted-sequence.rkt")

(provide :options-block)

(define-splicing-syntax-class :options-block
  #:attributes ([form 1] name)
  #:datum-literals (group)
  (pattern (~seq)
           #:with (form ...) '()
           #:with name #'#f)
  (pattern (~seq (_::block
                  (~and g (group #:name . _))
                  form
                  ...))
           #:cut
           #:with name (syntax-parse #'g
                         [(_ _ (_::block
                                (group n::dotted-identifier-sequence)))
                          (build-dot-symbol (syntax->list #'n) #:skip-dots? #t)]
                         [(_ _ n::dotted-identifier-sequence)
                          (build-dot-symbol (syntax->list #'n) #:skip-dots? #t)]
                         [_
                          (raise-syntax-error #f "invalid name form" #'g)]))
  (pattern (~seq (_::block form ...))
           #:with name #'#f))
