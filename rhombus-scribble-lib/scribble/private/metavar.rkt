#lang racket/base
(require (for-syntax racket/base))

(begin-for-syntax
  (provide (struct-out metavar))
  
  (struct metavar (id nonterm?)))
