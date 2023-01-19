#lang racket/base
(require (for-syntax racket/base)
         "definition.rkt"
         "space.rkt")

(begin-for-syntax
  (provide space+definition-transformer)
  (struct space+definition-transformer (sp def)
    #:property prop:space-name (lambda (self) (space+definition-transformer-sp self))
    #:property prop:definition-transformer (lambda (self) (space+definition-transformer-def self))))
