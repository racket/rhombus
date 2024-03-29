#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     "annotation-string.rkt")
         "binding.rkt"
         "static-info.rkt"
         "number.rkt")

(provide literal-infoer
         ;; useful for other binding patterns:
         literal-commit-nothing
         literal-bind-nothing)

(module+ for-info
  (provide (for-syntax (rename-out [string-static-infos indirect-string-static-infos]
                                   [bytes-static-infos indirect-bytes-static-infos])
                       install-literal-static-infos!
                       literal-static-infos)))

(define-syntax (literal-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos (~and datums (datum0 datum ...)))
     (binding-info (annotation-string-from-pattern
                    (apply string-append
                           (literal->string #'datum0)
                           (for/list ([datum (in-list (syntax->list #'(datum ...)))])
                             (string-append " || " (literal->string datum)))))
                   #'literal
                   ;; assumption: literal static infos are disjoint constants
                   (or (let ([si (literal-static-infos #'datum0)])
                         (and si
                              (for/and ([datum (in-list (syntax->list #'(datum ...)))])
                                (eq? (literal-static-infos datum) si))
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

(define-for-syntax string-static-infos #f)
(define-for-syntax bytes-static-infos #f)
(define-for-syntax char-static-infos #f)
(define-for-syntax (install-literal-static-infos! kind static-infos)
  (case kind
    [(string) (set! string-static-infos static-infos)]
    [(bytes) (set! bytes-static-infos static-infos)]
    [(char) (set! char-static-infos static-infos)]
    [else (error "unrecognized kind" kind)]))

(define-for-syntax (literal-static-infos d-stx)
  (define d (syntax-e d-stx))
  (or (and (string? d) string-static-infos)
      (and (bytes? d) bytes-static-infos)
      (and (exact-integer? d) int-static-infos)
      (and (flonum? d) flonum-static-infos)
      (and (rational? d) rational-static-infos)
      (and (real? d) real-static-infos)
      (and (number? d) number-static-infos)
      (and (char? d) char-static-infos)))
