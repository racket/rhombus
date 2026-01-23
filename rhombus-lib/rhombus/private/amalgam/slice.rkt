#lang racket/base
(require racket/fixnum
         "realm.rkt"
         "annotation-failure.rkt")

(provide slice-bounds)

(define (slice-bounds who what v len start end)
  (unless (exact-integer? start) (raise-annotation-failure who start "Int"))
  (unless (exact-integer? end) (raise-annotation-failure who end "Int"))
  (define s (if (< start 0) (+ start len) start))
  (define e (if (< end 0) (+ end len) end))
  (if (and (fixnum? s) (fixnum? e)
           (fx<= 0 s len)
           (fx<= s e len))
      (values s e)
      (raise-arguments-error* who rhombus-realm
                              "invalid slice bounds"
                              "given start" start
                              "given end" end
                              (string-append what " length") len
                              what v)))
