#lang s-exp "demod.rkt"
"amalgam-src.rkt" ; <- see for more info on the "amalgam" directory

;; Disable amalgam demodularization with the following keyword,
;; which may be useful during development to avoid having to
;; compile everything four times
#; #:no-demod

#:include (#:dir "amalgam"
           ;; Although Rhombus is implemented with these libraries,
           ;; that fact can be hidden from the outside:
           #:collect "enforest"
           #:collect "pretty-expressive"
           ;; We wouldn't want to duplicate the shrubbery parser but
           ;; that's used only lazily (i.e., dynamically) required
           ;; within the amalgam; it's ok to inline small shrubbery
           ;; utilities:
           #:collect "shrubbery"
           ;; Inline these utilities:
           #:collect "syntax"
           #:module racket/unsafe/ops
           #:module racket/math
           #:module racket/vector
           #:module racket/match
           #:collect "racket/match"
           #:module racket/mutability)

#:exclude (#:module syntax/parse/pre)

#:prune-definitions
