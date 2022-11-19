#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt")
         "printer-property.rkt"
         "name-root.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "realm.rkt"
         "class-dot.rkt")

(provide Printer
         (for-space rhombus/annotation Printer))

(define-values (prop:Printer Printer? Printer-ref)
  (make-struct-type-property 'Printer
                             #f
                             (list (cons prop:printer
                                         (lambda (v) bounce-to-printer-interface)))))
(define-values (prop:Printer-public Printer-public? Printer-public-ref)
  (make-struct-type-property 'Printer
                             #f
                             (list (cons prop:Printer
                                         (lambda (vt) vt)))))

(define (bounce-to-printer-interface v op mode)
  (if (eq? mode 'print)
      (print-internal-method v op)
      (display-internal-method v op)))

(define-syntax Printer-internal
  (interface-desc #'Printer-internal
                  #f
                  #'()
                  #'prop:Printer
                  #'Printer-ref
                  '#(#&print #&display)
                  #'#(#:abstract display_as_print)
                  (hasheq 'print 0
                          'display 1)
                  #hasheq()))

(define-name-root Printer
  #:root (interface-desc #'Printer
                         #'Printer-internal
                         #'()
                         #'prop:Printer-public
                         #'Printer-public-ref
                         '#(#&print #&display)
                         #'#(#:abstract display_as_print)
                         (hasheq 'print 0
                                 'display 1)
                         #hasheq())
  #:fields
  ([print print-method]
   [display display-mthod]))

(define-annotation-syntax Printer
  (identifier-annotation #'printer-interface #'Printer-public? #'((#%dot-provider printer-instance))))

(define-dot-provider-syntax printer-instance
  (dot-provider-more-static (make-handle-class-instance-dot #'Printer #hasheq() #hasheq())))

(define (display_as_print v op)
  (print-internal-method v op))

(define (get-printer who v)
  (define vt (Printer-public-ref v #f))
  (unless vt
    (raise-argument-error* who rhombus-realm "Printer" v))
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
  ((vector-ref (Printer-ref v) 0) v op))

(define (display-internal-method v op)
  ((vector-ref (Printer-ref v) 1) v op))
