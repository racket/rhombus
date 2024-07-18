#lang racket/base
(require (except-in "core.rkt" #%module-begin)
         (submod "core.rkt" amalgam-module-begin))

(provide (all-from-out "core.rkt")
         #%module-begin)
