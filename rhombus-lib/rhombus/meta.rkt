#lang racket/base
(require "private/bounce.rkt")

(bounce (submod "private/amalgam.rkt" core-meta)
        (submod "private/amalgam.rkt" core-meta sequence_meta))

(require (for-syntax (submod "private/amalgam.rkt" core-derived)))
(provide (for-syntax (all-from-out (submod "private/amalgam.rkt" core-derived))))

(bounce (submod "private/amalgam.rkt" core-meta class-meta)
        (submod "private/amalgam.rkt" core-meta interface-meta)
        (submod "private/amalgam.rkt" core-meta veneer-meta))

;; re-export `meta` for non-expression spaces,
;; otherwise these can get shadowed (in a sense)
;; by the `meta` expression export
(bounce #:only (meta) #:spaces (rhombus/impo rhombus/expo)
        (submod "private/amalgam.rkt" core))

(#%declare #:flatten-requires)
