#lang racket/base
(require "private/bounce.rkt"
         (only-in (submod "private/amalgam.rkt" parse) rhombus-definition)
         (only-in (submod "private/amalgam.rkt" dynamic-static) use_static))

(rhombus-definition (group use_static)) ;; defines `#%dynamism`

(bounce #:except (#%dynamism)
        "main.rkt")
(provide #%dynamism)

(module reader syntax/module-reader
  #:language '(lib "rhombus/static.rhm")
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info rhombus:get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (prefix-in rhombus: (submod "private/core.rkt" reader))))

(module configure-runtime racket/base
  (require rhombus/runtime-config))

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))
