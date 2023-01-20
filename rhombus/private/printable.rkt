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

(provide (for-spaces (rhombus/namespace
                      rhombus/class
                      rhombus/annot)
                     Printable))

(define-values (prop:Printable Printable? Printable-ref)
  (make-struct-type-property 'Printable
                             #f
                             (list (cons prop:printer
                                         (lambda (v) bounce-to-printer-interface)))))
(define-values (prop:Printable-public Printable-public? Printable-public-ref)
  (make-struct-type-property 'Printable
                             #f
                             (list (cons prop:Printable
                                         (lambda (vt) vt)))))

(define (bounce-to-printer-interface v op mode)
  (if (eq? mode 'print)
      (print-internal-method v op)
      (display-internal-method v op)))

(define-syntax Printable-internal
  (interface-desc #'Printable-internal
                  #f
                  #'()
                  #'prop:Printable
                  #'Printable-ref
                  '#(#&print #&display)
                  #'#(#:abstract display_as_print)
                  (hasheq 'print 0
                          'display 1)
                  #hasheq()
                  #t))

(define-class-desc-syntax Printable
  (interface-desc #'Printable
                  #'Printable-internal
                  #'()
                  #'prop:Printable-public
                  #'Printable-public-ref
                  '#(#&print #&display)
                  #'#(#:abstract display_as_print)
                  (hasheq 'print 0
                          'display 1)
                  #hasheq()
                  #t))

(define-name-root Printable
  #:fields
  ([print print-method]
   [display display-mthod]))

(define-annotation-syntax Printable
  (identifier-annotation #'Printable-public? #'((#%dot-provider printer-instance))))

(define-dot-provider-syntax printer-instance
  (dot-provider-more-static (make-handle-class-instance-dot #'Printable #hasheq() #hasheq())))

(define (display_as_print v op)
  (print-internal-method v op))

(define (get-printer who v)
  (define vt (Printable-public-ref v #f))
  (unless vt
    (raise-argument-error* who rhombus-realm "Printable" v))
  vt)

(define print-method
  (let ([print (lambda (v op)
                 ((vector-ref (get-printer 'print v) 0) v op))])
    print))
  
(define display-method
  (let ([print (lambda (v op)
                 ((vector-ref (get-printer 'display v) 1) v op))])
    print))

(define (print-internal-method v op)
  ((vector-ref (Printable-ref v) 0) v op))

(define (display-internal-method v op)
  ((vector-ref (Printable-ref v) 1) v op))
