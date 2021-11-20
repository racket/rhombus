#lang racket/base
(require "lex.rkt"
         "lex-comment.rkt"
         syntax-color/racket-lexer
         syntax-color/lexer-contract
         racket/symbol)

(provide shrubbery-lexer
         shrubbery-text-mode-lexer)

(define (shrubbery-lexer in pos status)
  (let-values ([(tok type paren start end backup status)
                (lex/comment/status in pos status racket-lexer*/status)])
    (define (to-string-or-eof tok)
      (cond
        [(eof-object? tok) tok]
        [(string? tok) tok]
        [(token? tok) (to-string-or-eof (token-e tok))]
        [(syntax? tok) (to-string-or-eof (syntax-e tok))]
        [(symbol? tok) (symbol->immutable-string tok)]
        [else "other"]))
    (values (to-string-or-eof tok) type paren start end backup
            (if (lex/comment-dont-stop-status? status)
                (dont-stop status)
                status))))

(define (shrubbery-text-mode-lexer in pos status)
  (shrubbery-lexer in pos (or status
                              (make-in-text-status))))
