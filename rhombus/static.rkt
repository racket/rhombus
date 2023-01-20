#lang racket/base
(require "private/bounce.rkt"
         (only-in "private/parse.rkt" rhombus-definition)
         (only-in "private/dynamic-static.rkt" use_static))

(rhombus-definition (group use_static))

(bounce #:except (|.| #%ref #%call)
        "main.rkt")
(bounce #:only (|.|)
        #:spaces (rhombus/impo rhombus/expo)
        "main.rkt")
(provide (for-space #f
                    |.|
                    #%ref
                    #%call)
         (for-space rhombus/repet
                    |.|
                    #%ref
                    #%call))

(module reader syntax/module-reader
  #:language 'rhombus/static
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info rhombus:get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (prefix-in rhombus: (submod "private/core.rkt" reader))))

(module configure-runtime racket/base
  (require rhombus/runtime-config))
