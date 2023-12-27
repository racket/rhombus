#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt")
         "provide.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         (only-in (submod "function-parse.rkt" for-call)
                  raise-result-failure))

(provide (for-spaces (rhombus/class)
                     Sequenceable))

(define-values (prop:Sequenceable Sequenceable? Sequenceable-ref)
  (make-struct-type-property
   'Sequenceable
   #false
   (list (cons prop:sequence (lambda (v)
                               (lambda (self)
                                 (define s ((vector-ref (Sequenceable-ref self) 0) self))
                                 (unless (sequence? s)
                                   (raise-result-failure 'to_sequence s "Sequence"))
                                 s))))))

(define-class-desc-syntax Sequenceable
  (interface-desc #'()
                  (vector-immutable (box-immutable 'to_sequence))
                  #'#(#:abstract)
                  (hasheq 'to_sequence 0)
                  #hasheq()
                  '()
                  #f
                  #'()
                  '()
                  ;; --------------------
                  #'Sequenceable
                  #'Sequenceable
                  #'prop:Sequenceable
                  #'prop:Sequenceable
                  #'Sequenceable-ref
                  #t
                  #f))
