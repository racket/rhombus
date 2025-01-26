#lang racket/base

(module reader syntax/module-reader
  #:language 'rhombus/private/amalgam/amalgam-core
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           shrubbery/private/lang)
  (provide get-info-proc)
  (define (get-info-proc key default make-default)
    (case key
      [(drracket:default-extension)
       "rhm"]
      [(drracket:define-popup)
       (dynamic-require 'rhombus/private/define-popup
                        'define-popup)]
      [else
       (shrubbery-get-info-proc/mode key default make-default)])))
