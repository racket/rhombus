#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt")
         "provide.rkt"
         "printer-property.rkt"
         "name-root.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "realm.rkt"
         "class-dot.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax))

(provide (for-spaces (rhombus/class)
                     Printable))

(define-values (prop:Printable Printable? Printable-ref)
  (make-struct-type-property 'Printable
                             #f
                             (list (cons prop:printer
                                         (lambda (v) bounce-to-printer-interface)))))

(define (bounce-to-printer-interface v op mode)
  (if (eq? mode 'print)
      (print-internal-method v op)
      (display-internal-method v op)))

(define-class-desc-syntax Printable
  (interface-desc #'Printable
                  #'Printable
                  #'()
                  #'prop:Printable
                  #'Printable-ref
                  '#(#&print #&display)
                  #'#(#:abstract display_as_print)
                  (hasheq 'print 1
                          'display 2)
                  #hasheq()
                  #t))

(define (display_as_print v op)
  (print-internal-method v op))

(define (get-printer who v)
  (define vt (Printable-ref v #f))
  (unless vt
    (raise-argument-error* who rhombus-realm "Printable" v))
  vt)

(define (print-internal-method v op)
  ((vector-ref (Printable-ref v) 0) v op))

(define (display-internal-method v op)
  ((vector-ref (Printable-ref v) 1) v op))
