#lang racket/base
(require (for-syntax racket/base)
         "private/bounce.rkt"
         "private/lang-helper.rkt")

(provide (rename-out [module-begin #%module-begin]))

(bounce #:except (#%module-begin)
        "main.rkt")

(module reader syntax/module-reader
  #:language '(lib "rhombus/lang_bridge.rhm")
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (only-in (submod "private/core.rkt" reader) get-info-proc)))

(module configure-runtime racket/base
  (require rhombus/runtime-config))

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))

(define-syntax (module-begin stx)
  (parse-module-begin stx 'lang_bridge))
