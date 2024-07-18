#lang racket/base
(require (for-syntax racket/base
                     "introducer.rkt"))

(provide (for-syntax in-dot-provider-space))

(begin-for-syntax
  (define in-dot-provider-space (make-interned-syntax-introducer/add 'rhombus/dot)))
