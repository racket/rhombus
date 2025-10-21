#lang racket/base
(require scribble/manual)

(provide double_shrub)

(define double_shrub
  (racketblock0
   (multi
    (group fun
           (parens (group x))
           (block (group x (op +) x))))))