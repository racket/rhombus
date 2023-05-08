#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt")
         "provide.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         (only-in "class-method-result.rkt" method-result))

(provide (for-spaces (rhombus/class)
                     Refable
                     MutableRefable))

(define-values (prop:Refable Refable? Refable-ref)
  (make-struct-type-property 'Refable
                             #f
                             ;; could have `prop:ref` in this list, but
                             ;; direct dispatch is set up in `class` when
                             ;; the interface is implemented, and that
                             ;; picks up the right static info
                             (list)))

(define-values (prop:MutableRefable MutableRefable? MutableRefable-ref)
  (make-struct-type-property 'Refable
                             #f
                             ;; similarly, could have `prop:ref+set` in
                             ;; this list, but direct dispatch is better
                             (list)))

(define-class-desc-syntax Refable
  (interface-desc #'Refable
                  #'Refable
                  #'()
                  #'prop:Refable
                  #'prop:Refable
                  #'Refable-ref
                  '#(#&ref)
                  #'#(#:abstract)
                  (hasheq 'ref 0)
                  #hasheq()
                  #t
                  '()
                  #f
                  #'()
                  '(ref)))

(define-class-desc-syntax MutableRefable
  (interface-desc #'MutableRefable
                  #'MutableRefable
                  #'(Refable)
                  #'prop:MutableRefable
                  #'prop:MutableRefable
                  #'MutableRefable-ref
                  '#(#&ref #&set)
                  #'#(#:abstract #:abstract)
                  (hasheq 'ref 0
                          'set 1)
                  (hasheq 'set #'void-result)
                  #t
                  '()
                  #f
                  #'()
                  '(ref set)))

(define-syntax void-result
  (method-result #'void? #t #'() 0))
