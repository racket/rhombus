#lang racket/base
(require "version-case.rkt")

(meta-if-version-at-least
 "8.12.0.7"
 (begin
   (require racket/mutable-treelist)
   (provide (all-from-out racket/mutable-treelist)))
 (begin
   (require "mutable-treelist-copy.rkt")
   (provide (all-from-out "mutable-treelist-copy.rkt"))))
