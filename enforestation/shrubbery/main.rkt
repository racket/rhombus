#lang racket/base

(module reader racket/base
  (require syntax/strip-context
           "parse.rkt")
  
  (provide (rename-out [shrubbery-read read]
                       [shrubbery-read-syntax read-syntax])
           get-info)

    (define (shrubbery-read in)
      (syntax->datum
       (shrubbery-read-syntax #f in)))
 
  (define (shrubbery-read-syntax src in)
    (strip-context
     #`(module anything racket/base
         '#,(parse-all in))))
 
  (define (get-info in mod line col pos)
    (lambda (key default)
      (case key
        [(color-lexer)
         (dynamic-require 'shrubbery/syntax-color
                          'shrubbery-lexer)]
        [(drracket:indentation)
         (dynamic-require 'shrubbery/indentation
                          'shrubbery-indentation)]
        [(drracket:range-indentation)
         (dynamic-require 'shrubbery/indentation
                          'shrubbery-range-indentation)]
        [(drracket:grouping-position)
         (dynamic-require 'shrubbery/navigation
                          'shrubbery-grouping-position)]
        [else default]))))
