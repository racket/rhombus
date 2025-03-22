#lang racket/base

(provide (struct-out definition-context))

(struct definition-context (def-ctx expand-context params-box)
  #:reflection-name 'DefinitionContext)
