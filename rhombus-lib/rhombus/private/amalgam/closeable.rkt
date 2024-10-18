#lang racket/base

(require (for-syntax racket/base
                     "interface-parse.rkt")
         (submod "annotation.rkt" for-class)
         "class-desc.rkt"
         "provide.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/class)
                     Closeable))

(define-values (prop:Closeable Closeable? Closeable-ref)
  (make-struct-type-property 'Closeable #f '()))

(define-annotation-syntax Closeable
  (identifier-annotation closeable? ()))
(define (closeable? v)
  (or (port? v)
      (Closeable? v)))

(define-class-desc-syntax Closeable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'()
                     '#(#&close)
                     #'#(#:abstract)
                     (hasheq 'describe 0)
                     (hasheq 'describe #f)
                     '()
                     #f
                     #'()
                     '()
                     ;; --------------------
                     #'Closeable
                     #'Closeable
                     #'prop:Closeable
                     #'prop:Closeable
                     #'Closeable-ref
                     #'Closeable-ref
                     #t
                     #f
                     null))))