#lang racket/base
(require (only-in "class-parse.rkt"
                  objects-desc))

(provide (struct-out veneer-desc)
         veneer-desc-ref)

(struct veneer-desc objects-desc
  (id
   super-id
   predicate-id    ; #f if not checked
   convert-id      ; #f if predicate-based
   instance-dot-providers)) ; `#%dot-provider` value, only for non-final


(define (veneer-desc-ref v) (and (veneer-desc? v) v))
