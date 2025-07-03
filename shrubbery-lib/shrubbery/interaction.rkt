#lang racket/base
(require "lex.rkt"
         "variant.rkt"
         "private/paren.rkt"
         syntax-color/racket-lexer)

(provide shrubbery-submit-predicate
         make-shrubbery-submit-predicate)

(define (shrubbery-submit-predicate in whitespace-after? #:variant [variant default-variant])
  (and whitespace-after?
       ;; siilar to `lex-all` with `#:interactive? #t`, but staying in lexer
       ;; land by using `racket-lexer/status`:
       (let loop ([prev-status 'initial] [something? #f] [depth 0] [blanks 0] [multi? #f])
         (let-values ([(tok type paren start end backup status)
                       (lex/status in (file-position in) prev-status racket-lexer*/status)])
           (case (cond
                   [(eof-object? tok) 'EOF]
                   [(token? tok) (token-name tok)]
                   [else 's-exp])
             [(EOF) (and (not (lex-nested-status? prev-status))
                         something?
                         (zero? depth)
                         (or (not multi?)
                             (blanks . > . 0)))]
             [(fail) #f]
             [(opener) (loop status #t (+ depth 1) 0 multi?)]
             [(closer) (loop status #t (max 0 (- depth 1)) 0 multi?)]
             [(comment) (loop status something? depth 0 multi?)]
             [(whitespace)
              (define lines (count-newlines (syntax-e (token-value tok))))
              (loop status something? depth (+ lines blanks) multi?)]
             [(block-operator semicolon-operator)
              (loop status #t depth 0
                    (or multi? (and (not (lex-nested-status? prev-status))
                                    (zero? depth))))]
             [else (loop status #t depth 0 multi?)])))))

(define (make-shrubbery-submit-predicate #:variant [variant default-variant])
  (lambda (in whitespace-after?)
    (shrubbery-submit-predicate in whitespace-after? #:variant variant)))

(define (count-newlines s)
  (for/sum ([c (in-string s)])
    (if (char=? c #\newline)
        1
        0)))
