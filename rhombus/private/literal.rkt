#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print)
         "binding.rkt"
         "parse.rkt")

(provide literal-infoer
         ;; useful for other binding patterns:
         literal-commit-nothing
         literal-bind-nothing)

(define-syntax (literal-infoer stx)
  (syntax-parse stx
    [(_ static-infos (datum ...))
     (binding-info (let loop ([l '("matching(")]
                              [datums (syntax->list #'(datum ...))]
                              [first? #t])
                     (if (null? datums)
                         (apply string-append (reverse (cons ")" l)))
                         (loop (cons (shrubbery-syntax->string (car datums))
                                     (if first?
                                         l
                                         (cons "||" l)))
                               (cdr datums)
                               #f)))
                   #'literal
                   #'static-infos
                   #'()
                   #'literal-matcher
                   #'literal-commit-nothing
                   #'literal-bind-nothing
                   #'(datum ...))]))

(define-syntax (literal-matcher stx)
  (syntax-parse stx
    [(_ arg-id (datum ...) IF success fail)
     #'(IF (or (equal-always? arg-id (quote datum))
               ...)
           success
           fail)]))

(define-syntax (literal-commit-nothing stx)
  (syntax-parse stx
    [(_ arg-id datums)
     #'(begin)]))

(define-syntax (literal-bind-nothing stx)
  (syntax-parse stx
    [(_ arg-id datums)
     #'(begin)]))
