#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     "annotation-string.rkt")
         "binding.rkt"
         "static-info.rkt"
         (submod "static-info.rkt" for-literal))

(provide literal-infoer
         ;; useful for other binding patterns:
         literal-commit-nothing
         literal-bind-nothing)

(define-syntax (literal-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos (~and datums (datum0 datum ...)))
     (binding-info (annotation-string-from-pattern
                    (apply string-append
                           (literal->string #'datum0)
                           (for/list ([datum (in-list (syntax->list #'(datum ...)))])
                             (string-append " || " (literal->string datum)))))
                   #'literal
                   ;; assumption: quoted static infos are disjoint constants
                   (or (let ([si (quoted-static-infos #'datum0)])
                         (and si
                              (for/and ([datum (in-list (syntax->list #'(datum ...)))])
                                (eq? (quoted-static-infos datum) si))
                              (static-infos-union si #'up-static-infos)))
                       #'up-static-infos)
                   #'()
                   #'literal-matcher
                   #'literal-commit-nothing
                   #'literal-bind-nothing
                   #'datums)]))

(define-for-syntax (literal->string d-stx)
  (define d (syntax-e d-stx))
  (define str (shrubbery-syntax->string d-stx))
  (if (or (symbol? d)
          (keyword? d))
      (string-append "#'" str)
      str))

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
