#lang racket/base

(module reader racket/base
  (require syntax/strip-context
           "parse.rkt")
  
  (provide (rename-out [shrubbery-read read]
                       [shrubbery-read-syntax read-syntax])
           get-info
           get-info-proc)

    (define (shrubbery-read in)
      (syntax->datum
       (shrubbery-read-syntax #f in)))
 
  (define (shrubbery-read-syntax src in)
    (strip-context
     #`(module anything racket/base
         '#,(parse-all in))))
 
  (define (get-info in mod line col pos)
    (lambda (key default)
      (get-info-proc key default (lambda (key default) default))))

  (define (get-info-proc key default make-default)
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
      [(drracket:paren-matches)
       (dynamic-require 'shrubbery/indentation
                        'shrubbery-paren-matches)]
      [(drracket:grouping-position)
       (dynamic-require 'shrubbery/navigation
                        'shrubbery-grouping-position)]
      [else (make-default key default)])))
