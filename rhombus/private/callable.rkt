#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt")
         "provide.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax))

(provide (for-spaces (rhombus/class)
                     Callable))

(define-values (prop:Callable Callable? Callable-ref)
  (make-struct-type-property 'Callable
                             #f
                             ;; prop:procedure should be in this list, but
                             ;; we have to handle it more directly
                             ;; to get the arity right; direct handling also
                             ;; lets us supply the method-as-function directly
                             ;; as the property value; direct handling is
                             ;; implemented by `callable-method-as-property`
                             ;; below
                             (list)))

(define-for-syntax callable-interface-desc
  (interface-desc #'Callable
                  #'Callable
                  #'()
                  #'prop:Callable
                  #'prop:Callable
                  #'Callable-ref
                  '#(#&call)
                  #'#(#:abstract)
                  (hasheq 'call 0)
                  #hasheq()
                  #t
                  '()
                  #f
                  '(call)))

(define-class-desc-syntax Callable
  callable-interface-desc)
