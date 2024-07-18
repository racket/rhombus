#lang racket/base

(module reader racket/base
  (require syntax/strip-context
           syntax/stx
           "parse.rkt"
           "private/module-path.rkt")
  
  (provide (rename-out [shrubbery-read read]
                       [shrubbery-read-syntax read-syntax])
           get-info
           get-info-proc)

  (define (shrubbery-read in)
    (syntax->datum
     (shrubbery-read-syntax #f in)))

  (define (shrubbery-read-syntax src in)
    ;; If there's something on the first line, use that as a module
    ;; language. Otherwise, just quote. Detect whether there are
    ;; any newlines by checking the prefix of the first group.
    (define-values (line col pos) (port-next-location in))
    (define lang (parse-all in #:mode 'line))
    (define shrubbery (parse-all in))
    (strip-context
     (cond
       [(and (not (eof-object? lang))
             (not (stx-null? (stx-cdr lang))))
        #`(module anything #,(parse-module-path-as-shrubbery lang)
            #,shrubbery)]
       [else
        #`(module anything racket/base
            '#,shrubbery)])))

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
      [(drracket:quote-matches)
       (dynamic-require 'shrubbery/indentation
                        'shrubbery-quote-matches)]
      [(drracket:grouping-position)
       (dynamic-require 'shrubbery/navigation
                        'shrubbery-grouping-position)]
      [(drracket:submit-predicate)
       (dynamic-require 'shrubbery/interaction
                        'shrubbery-submit-predicate)]
      [(drracket:keystrokes)
       (dynamic-require 'shrubbery/keystroke
                        'shrubbery-keystrokes)]
      [(drracket:comment-delimiters)
       '((line "//" " ")
         (region "/*" " *" "*/" " "))]
      [else (make-default key default)])))
