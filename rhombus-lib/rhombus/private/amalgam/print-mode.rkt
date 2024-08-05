#lang racket/base
(require "enum.rkt"
         "provide.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     PrintMode))

(define-simple-symbol-enum PrintMode
  text
  expr)
