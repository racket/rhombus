#lang racket/base
(require "private/bounce.rkt"
         (only-in "private/parse.rkt" rhombus-definition)
         (only-in "private/dynamic-static.rkt" use_static))

(rhombus-definition (group use_static))

(bounce #:except (|.| #%ref)
        "main.rkt")
(provide |.|
         #%ref)

(module reader syntax/module-reader
  #:language 'rhombus/static
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info rhombus:get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (prefix-in rhombus: (submod "main.rkt" reader))))

(module configure-runtime racket/base
  (require rhombus/runtime-config))
