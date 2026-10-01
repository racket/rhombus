#lang racket/base
(require ffi2
         racket/flonum
         (only-in '#%foreign
                  flvector->cpointer
                  cpointer->ffi2-ptr))

(provide flonum_array_ptr_t)

(define (flvector->pointer fl)
  (cpointer->ffi2-ptr #f (flvector->cpointer fl)))

(define-ffi2-type flonum_array_ptr_t ptr_t
  #:predicate flvector?
  #:racket->c flvector->pointer
  #:c->racket (lambda (p)
                (raise-arguments-error 'flvector_array_ptr_t "cannot convert a pointer object to a flonum array"
                                       "pointer" p)))
