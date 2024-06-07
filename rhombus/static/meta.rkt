#lang racket/base
(require "../private/bounce.rkt"
         (for-syntax
          racket/base
          (only-in (submod "../private/amalgam.rkt" parse) rhombus-definition)
          (only-in (submod "../private/amalgam.rkt" dynamic-static) use_static)))

(begin-for-syntax
  (rhombus-definition (group use_static)))

(bounce #:except (#%dynamism)
        "../meta.rkt")
(provide (for-syntax
          #%dynamism))
