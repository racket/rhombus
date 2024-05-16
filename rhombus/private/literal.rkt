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
  (provide (for-syntax (rename-out [get-string-static-infos indirect-get-string-static-infos]
                                   [get-bytes-static-infos indirect-get-bytes-static-infos])
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
                   (static-infos-union
                    (or (for/fold ([si (literal-static-infos #'datum0)])
                                  ([datum (in-list (syntax->list #'(datum ...)))])
                          (and si (static-infos-intersect si (literal-static-infos datum))))
                        #'())
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

(define-for-syntax get-string-static-infos #f)
(define-for-syntax get-bytes-static-infos #f)
(define-for-syntax get-char-static-infos #f)
(define-for-syntax (install-literal-static-infos! kind get-static-infos)
  (case kind
    [(string) (set! get-string-static-infos get-static-infos)]
    [(bytes) (set! get-bytes-static-infos get-static-infos)]
    [(char) (set! get-char-static-infos get-static-infos)]
    [else (error "unrecognized kind" kind)]))

(define-for-syntax (literal-static-infos d-stx)
  (define d (syntax-e d-stx))
  (or (and (string? d) (get-string-static-infos))
      (and (bytes? d) (get-bytes-static-infos))
      (and (exact-integer? d) (get-int-static-infos))
      (and (flonum? d) (get-flonum-static-infos))
      (and (rational? d) (get-rational-static-infos))
      (and (real? d) (get-real-static-infos))
      (and (number? d) (get-number-static-infos))
      (and (char? d) (get-char-static-infos))))
