#lang racket/base

(provide (struct-out spacer-binding))

(struct spacer-binding (datum annot-b ns-b)
  #:prefab)
