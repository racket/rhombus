#lang racket/base
(require syntax/strip-context
         syntax/stx
         "../parse.rkt"
         "module-path.rkt")

(provide shrubbery-read-syntax/mode
         shrubbery-get-info-proc/mode)

(define (shrubbery-read-syntax/mode src in
                                    #:mode [mode 'top])
  ;; If there's something on the first line, use that as a module
  ;; language. Otherwise, just quote. Detect whether there are
  ;; any newlines by checking the prefix of the first group.
  (define-values (line col pos) (port-next-location in))
  (define lang (parse-all in #:mode 'line))
  (define shrubbery (parse-all in #:mode mode #:source src))
  (define p-name (object-name in))
  (define module-name
    (if (path? p-name)
        (let-values ([(base name dir?) (split-path p-name)])
          (string->symbol
           (path->string (path-replace-extension name #""))))
        'anonymous-module))
  (strip-context
   (cond
     [(and (not (eof-object? lang))
           (not (stx-null? (stx-cdr lang))))
      #`(module #,module-name #,(parse-module-path-as-shrubbery lang)
          #,shrubbery)]
     [else
      #`(module #,module-name racket/base
          '#,shrubbery)])))

(define (shrubbery-get-info-proc/mode key default make-default
                                      #:mode [mode 'top])
  (case key
    [(color-lexer)
     (dynamic-require 'shrubbery/syntax-color
                      (case mode
                        [(top) 'shrubbery-lexer]
                        [(text) 'shrubbery-text-mode-lexer]
                        [else (error "invalid mode")]))]
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
    [else
     (make-default key default)]))
