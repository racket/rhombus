#lang racket/base
(require rhombus/private/version-case)

(meta-if-version-at-least
 "8.14.0.5" ; assuming implies "scribble-lib" version 1.54
 (require scribble/manual-struct)
 (define (desc-extras/c v) #t))

(provide desc-extras/c)

