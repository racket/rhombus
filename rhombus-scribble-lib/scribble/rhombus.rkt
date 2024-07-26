#lang racket/base

(require rhombus/scribble)
(provide (all-from-out rhombus/scribble))

(module reader racket/base
  (require (submod rhombus/scribble reader))
  (provide (all-from-out (submod rhombus/scribble reader))))

(module configure-expand racket/base
  (require (submod rhombus/scribble configure-expand))
  (provide (all-from-out (submod rhombus/scribble configure-expand))))
