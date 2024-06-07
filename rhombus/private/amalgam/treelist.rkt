#lang racket/base
(require "../version-case.rkt")

(meta-if-version-at-least
 "8.12.0.7"
 (begin
   (require racket/treelist)
   (provide (all-from-out racket/treelist)))
 (begin
   (require "treelist-copy.rkt")
   (provide (all-from-out "treelist-copy.rkt"))))

(module unsafe racket/base
  (require "../version-case.rkt")
  
  (meta-if-version-at-least
   "8.12.0.7"
   (begin
     (require (submod racket/treelist unsafe))
     (provide (all-from-out (submod racket/treelist unsafe))))
   (begin
     (require (submod "treelist-copy.rkt" unsafe))
     (provide (all-from-out (submod "treelist-copy.rkt" unsafe))))))
