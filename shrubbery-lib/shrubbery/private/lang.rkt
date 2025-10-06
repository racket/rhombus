#lang racket/base
(require syntax/strip-context
         syntax/stx
         "../variant.rkt"
         "../parse.rkt"
         "module-path.rkt")

(provide shrubbery-read-syntax/mode
         shrubbery-get-info-proc/mode)

(define (shrubbery-read-syntax/mode src in
                                    #:mode [mode 'top]
                                    #:variant [variant default-variant])
  ;; If there's something on the first line, use that as a module
  ;; language. Otherwise, just quote. Detect whether there are
  ;; any newlines by checking the prefix of the first group.
  (define-values (line col pos) (port-next-location in))
  (define lang (parse-all in #:mode 'line #:variant variant))
  (define-values (content-line content-col content-pos) (port-next-location in))
  (define shrubbery (parse-all in #:mode mode #:source src #:variant variant))
  (define p-name (object-name in))
  (define module-name
    (if (path? p-name)
        (let-values ([(base name dir?) (split-path p-name)])
          (string->symbol
           (path->string (path-replace-extension name #""))))
        'anonymous-module))
  (define (add-srcloc stx)
    (datum->syntax #f (syntax-e stx) (vector src line col pos
                                             (and pos content-pos (- content-pos pos)))))
  (add-srcloc
   (strip-context
    (cond
      [(and (not (eof-object? lang))
            (not (stx-null? (stx-cdr lang))))
       #`(module #,module-name #,(parse-module-path-as-shrubbery lang)
           #,shrubbery)]
      [else
       #`(module #,module-name racket/base
           (require racket/pretty)
           (pretty-write '#,shrubbery))]))))

(define (shrubbery-get-info-proc/mode key default make-default
                                      #:mode [mode 'top]
                                      #:variant [variant default-variant])
  (case key
    [(color-lexer)
     ((dynamic-require 'shrubbery/syntax-color
                       (case mode
                         [(top) 'make-shrubbery-lexer]
                         [(text) 'make-shrubbery-text-mode-lexer]
                         [else (error "invalid mode")]))
      #:variant variant)]
    [(drracket:indentation)
     ((dynamic-require 'shrubbery/indentation
                       'make-shrubbery-indentation)
      #:variant variant)]
    [(drracket:range-indentation)
     ((dynamic-require 'shrubbery/indentation
                       'make-shrubbery-range-indentation)
      #:variant variant)]
    [(drracket:range-indentation/reverse-choices)
     ((dynamic-require 'shrubbery/indentation
                       'make-shrubbery-range-indentation/reverse-choices)
      #:variant variant)]
    [(drracket:paren-matches)
     ((dynamic-require 'shrubbery/indentation
                       'make-shrubbery-paren-matches)
      #:variant variant)]
    [(drracket:quote-matches)
     ((dynamic-require 'shrubbery/indentation
                       'make-shrubbery-quote-matches)
      #:variant variant)]
    [(drracket:grouping-position)
     ((dynamic-require 'shrubbery/navigation
                       'make-shrubbery-grouping-position)
      #:variant variant)]
    [(drracket:submit-predicate)
     ((dynamic-require 'shrubbery/interaction
                       'make-shrubbery-submit-predicate)
      #:variant variant)]
    [(drracket:keystrokes)
     ((dynamic-require 'shrubbery/keystroke
                       'make-shrubbery-keystrokes)
      #:variant variant)]
    [(drracket:comment-delimiters)
     '((line "//" " ")
       (region "/*" " *" "*/" " "))]
    [else
     (make-default key default)]))
