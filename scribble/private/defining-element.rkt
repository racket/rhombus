#lang racket/base
(require scribble/core)

;; defining-element is recognized in "line-shape.rkt"
;; as used by "typeset-rhombus.rkt"

(provide (struct-out defining-element))

(struct defining-element element())
