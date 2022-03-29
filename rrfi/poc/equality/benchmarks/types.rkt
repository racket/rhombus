#lang racket/base

(provide (struct-out benchmark))

(struct benchmark (name load)
  #:transparent)
