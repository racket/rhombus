#lang racket/base
(require (submod "private/amalgam.rkt" runtime-config))

(#%declare #:flatten-requires)

(void (install-runtime-config!))
