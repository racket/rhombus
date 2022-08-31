#lang racket/base
(require "private/bounce.rkt")

(bounce "main.rkt"
        "meta.rkt")

(module reader syntax/module-reader
  #:language 'rhombus/and_meta
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info rhombus:get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (only-in (submod shrubbery reader)
                    get-info-proc)
           (prefix-in rhombus: (submod "main.rkt" reader))))

(module configure-runtime racket/base
  (require rhombus/runtime-config))
