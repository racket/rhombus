#lang racket/base
(require "../private/bounce.rkt"
         (for-syntax
          racket/base
          (only-in "../private/parse.rkt" rhombus-definition)
          (only-in "../private/dynamic-static.rkt" use_static)))

(begin-for-syntax
  (rhombus-definition (group use_static)))

(bounce #:except (|.| #%ref)
        "../meta.rkt")
(provide (for-syntax |.|
                     #%ref))
