#lang racket/base

(provide port-pipe?)

(define (port-pipe? v)
  (and (port? v)
       (with-handlers ([exn:fail? (lambda (x) #f)])
         (pipe-content-length v))))

