#lang racket/base
(require "../version-case.rkt")

(provide pipe-port?)

(meta-if-version-at-least
 "8.15.0.9"
 (void)
 (define (pipe-port? v)
   (and (port? v)
        (with-handlers ([exn:fail? (lambda (x) #f)])
          (pipe-content-length v)))))
