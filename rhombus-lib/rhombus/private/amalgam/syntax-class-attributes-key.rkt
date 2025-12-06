#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(provide (for-syntax set-or-and-syntax-class-attributes!))

(begin-for-syntax
 (define or-syntax-class-attributes (lambda (a b) a))
 (define and-syntax-class-attributes (lambda (a b) #'()))

 (define (set-or-and-syntax-class-attributes! or-proc and-proc)
   (set! or-syntax-class-attributes or-proc)
   (set! and-syntax-class-attributes and-proc)))

(define-static-info-key-syntax/provide #%syntax-class-attributes
  (static-info-key (lambda (a b)
                     (or-syntax-class-attributes a b))
                   (lambda (a b)
                     (and-syntax-class-attributes a b))))
