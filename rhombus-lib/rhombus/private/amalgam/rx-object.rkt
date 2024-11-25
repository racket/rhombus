#lang racket/base

(provide rx?
         rx-regexp
         set-rx!)

(define rx? not)
(define rx-regexp values)

(define (set-rx! pred sel)
  (set! rx? pred)
  (set! rx-regexp sel))
