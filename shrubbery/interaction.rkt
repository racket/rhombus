#lang racket/base
(require "lex.rkt"
         "private/paren.rkt"
         syntax-color/racket-lexer)

(provide shrubbery-submit-predicate)

(define (shrubbery-submit-predicate in whitespace-after?)
  (and whitespace-after?
       ;; siilar to `lex-all` with `#:interactive? #t`, but staying in lexer
       ;; land by using `racket-lexer/status`:
       (let loop ([prev-status 'initial] [something? #f] [depth 0] [blanks 0] [block? #f])
         (let-values ([(tok type paren start end backup status)
                       (lex/status in (file-position in) prev-status racket-lexer/status)])
           (case (cond
                   [(eof-object? tok) 'EOF]
                   [(token? tok) (token-name tok)]
                   [else 's-exp])
             [(EOF) (and (not (lex-nested-status? prev-status))
                         something?
                         (zero? depth)
                         (or (not block?)
                             (blanks . > . 0)))]
             [(fail) #f]
             [(opener) (loop status #t (+ depth 1) 0 block?)]
             [(closer) (loop status #t (max 0 (- depth 1)) 0 block?)]
             [(comment semicolon-operator) (loop status something? depth 0 block?)]
             [(whitespace)
              (define lines (count-newlines (syntax-e (token-value tok))))
              (loop status something? depth (+ lines blanks) block?)]
             [(block-operator) (loop status #t depth 0
                                     (or block? (and (not (lex-nested-status? prev-status))
                                                     (zero? depth))))]
             [else (loop status #t depth 0 block?)])))))

(define (count-newlines s)
  (for/sum ([c (in-string s)])
    (if (char=? c #\newline)
        1
        0)))
