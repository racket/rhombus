#lang racket/base
(require "private/bounce.rkt")

(bounce (submod "private/amalgam.rkt" core)
        (submod "private/amalgam.rkt" core-macro)
        (submod "private/amalgam.rkt" core-derived))

(#%declare #:flatten-requires)

(module reader syntax/module-reader
  #:language '(lib "rhombus/main.rhm")
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info get-info-proc
  #:whole-body-readers? #t
  (provide get-info-proc)
  (require shrubbery/parse
           (only-in (submod "private/core.rkt" reader) get-info-proc)))

(module configure-runtime racket/base
  (require rhombus/runtime-config))

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))
