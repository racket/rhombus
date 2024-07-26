#lang racket/base

(require rhombus/scribble/manual)
(provide (all-from-out rhombus/scribble/manual))

(module reader racket/base
  (require (submod rhombus/scribble/manual reader))
  (provide (all-from-out (submod rhombus/scribble/manual reader))))

(module configure-expand racket/base
  (require (submod rhombus/scribble/manual configure-expand))
  (provide (all-from-out (submod rhombus/scribble/manual configure-expand))))
