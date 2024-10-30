#lang racket/base

(require (for-syntax racket/base
                     "interface-parse.rkt"
                     "class-method-result.rkt")
         "define-arity.rkt"
         (submod "annotation.rkt" for-class)
         "class-desc.rkt"
         "provide.rkt"
         (submod "dot.rkt" for-dot-provider)
         "dot-provider-key.rkt"
         "annotation-failure.rkt"
         "dot-parse.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/class)
                     Closeable))

(define-values (prop:Closeable Closeable? Closeable-ref)
  (make-struct-type-property 'Closeable #f '()))

(define-annotation-syntax Closeable
  (identifier-annotation closeable? ((#%dot-provider closeable-instance))))
(define (closeable? v)
  (or (port? v)
      (Closeable? v)))

(define-class-desc-syntax Closeable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'()
                     '#(#&close)
                     #'#(#:abstract)
                     (hasheq 'close 0)
                     (hasheq 'close #'close-result)
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

(define-syntax close-result
  (method-result-maker
   (lambda ()
     (method-result #f #t 0 "Any" #'() 1))))

(define-dot-provider-syntax closeable-instance
  (dot-provider
   (dot-parse-dispatch
    (lambda (field-sym field-proc ary nary repetition? fail-k)
      (case field-sym
        [(close) (nary 1 #'Closeable.close #'Closeable.close/method)]
        [else (fail-k)])))))

(define/method (Closeable.close v)
  (cond
    [(input-port? v) (close-input-port v)]
    [(output-port? v) (close-output-port v)]
    [(Closeable? v) ((vector-ref (Closeable-ref v) 0) v)]
    [else (raise-annotation-failure 'Closeable.close v "Closeable")]))
