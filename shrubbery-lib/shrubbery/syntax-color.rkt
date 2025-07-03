#lang racket/base
(require syntax-color/racket-lexer
         syntax-color/lexer-contract
         racket/symbol
         "lex.rkt"
         "lex-open-close.rkt"
         "variant.rkt")

(provide shrubbery-lexer
         shrubbery-text-mode-lexer
         make-shrubbery-lexer
         make-shrubbery-text-mode-lexer)

(define (shrubbery-lexer in pos status #:variant [variant default-variant])
  (let-values ([(tok type paren start end backup status)
                (lex/open-close/status in pos status racket-lexer*/status #:variant variant)])
    (define (to-string-or-eof tok)
      (cond
        [(eof-object? tok) tok]
        [(string? tok) tok]
        [(token? tok) (to-string-or-eof (token-e tok))]
        [(syntax? tok) (to-string-or-eof (syntax-e tok))]
        [(symbol? tok) (symbol->immutable-string tok)]
        [else "other"]))
    (values (to-string-or-eof tok) type paren start end backup
            (if (lex/open-close-dont-stop-status? status)
                (dont-stop status)
                status))))

(define (shrubbery-text-mode-lexer in pos status #:variant [variant default-variant])
  (shrubbery-lexer in pos (or status
                              (make-in-text-status))
                   #:variant variant))

(define (make-shrubbery-lexer #:variant [variant default-variant])
  (lambda (in pos status)
    (shrubbery-lexer in pos status #:variant variant)))

(define (make-shrubbery-text-mode-lexer #:variant [variant default-variant])
  (lambda (in pos status)
    (shrubbery-text-mode-lexer in pos status #:variant variant)))
