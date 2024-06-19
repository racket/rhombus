#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt"
                     "class-method-result.rkt")
         "provide.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         "sequence-constructor-key.rkt")

(provide (for-spaces (rhombus/class)
                     Sequenceable))

(define-values (prop:Sequenceable Sequenceable? Sequenceable-ref)
  (make-struct-type-property
   'Sequenceable
   #false
   (list (cons prop:sequence (lambda (v)
                               (lambda (self)
                                 ((vector-ref (Sequenceable-ref self) 0) self)))))))

(define-class-desc-syntax Sequenceable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'()
                     '#(#&to_sequence)
                     #'#(#:abstract)
                     (hasheq 'to_sequence 0)
                     (hasheq 'to_sequence #'to-sequence-result)
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
                     #f
                     null))))

(define-syntax to-sequence-result
  (method-result-maker
   (lambda ()
     (method-result #'sequence? #t 1 "Sequence" #'((#%sequence-constructor #t)) 2))))
