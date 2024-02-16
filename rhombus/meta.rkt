#lang racket/base
(require "private/bounce.rkt")

(bounce "private/core-meta.rkt"
        "private/sequence_meta.rhm")

(require (for-syntax "private/core-derived.rkt"))
(provide (for-syntax (all-from-out "private/core-derived.rkt")))

(bounce-meta "private/class-meta.rkt"
             "private/interface-meta.rkt"
             "private/veneer-meta.rkt")

;; re-export `meta` for non-expression spaces,
;; otherwise these can get shadowed (in a sense)
;; by the `meta` expression export
(bounce #:only (meta) #:spaces (rhombus/impo rhombus/expo)
        "private/core.rkt")
