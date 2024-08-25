#lang racket/base
(require (submod "private/amalgam.rkt" runtime-config)
         "private/version-case.rkt")

(meta-if-version-at-least
 "8.13.0.4"
 (#%declare #:flatten-requires)
 (void))

(void (install-runtime-config!))
