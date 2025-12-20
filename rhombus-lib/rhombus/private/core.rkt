#lang racket/base

(module reader syntax/module-reader
  #:language 'rhombus/private/amalgam/amalgam-core
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           shrubbery/private/lang
           shrubbery/variant)
  (provide get-info-proc
           make-get-info-proc
           documentation-language-family-hash)
  (define (get-info-proc key default make-default
                         #:variant [variant default-variant]
                         #:semantic-type-guess [semantic-type-guess default-semantic-type-guess])
    (case key
      [(color-lexer)
       ((dynamic-require 'rhombus/private/syntax-color 'make-rhombus-lexer)
        #:variant variant
        #:semantic-type-guess semantic-type-guess)]
      [(drracket:default-extension)
       "rhm"]
      [(drracket:define-popup)
       (dynamic-require 'rhombus/private/define-popup
                        'define-popup)]
      [(documentation-language-family)
       documentation-language-family-hash]
      [else
       (shrubbery-get-info-proc/mode key default make-default #:variant variant)]))
  (define (make-get-info-proc #:variant [variant default-variant]
                              #:semantic-type-guess [semantic-type-guess default-semantic-type-guess])
    (lambda (key default make-default)
      (get-info-proc key default make-default
                     #:variant variant
                     #:semantic-type-guess semantic-type-guess)))
  (define (default-semantic-type-guess str default)
    (default str))
  (define documentation-language-family-hash
    (hash 'doc-language-name "Rhombus"
          'doc-path "rhombus/index.html"
          'doc-query
          (hash 'fam "Rhombus" 'famroot "rhombus"))))
