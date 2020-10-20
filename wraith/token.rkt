#lang racket/base

(provide (struct-out token)
         read-token
         read-tokens
         string->tokens)

(require racket/bool
         racket/port
         syntax-color/racket-lexer)
(module+ test
  (require rackunit))

;; A Token is a (token String TokenType Srcloc)
(struct token [string type srcloc] #:transparent)

;; A TokenType is one of:
;;  - 'error
;;  - 'comment
;;  - 'sexp-comment
;;  - 'white-space
;;  - 'constant
;;  - 'string
;;  - 'no-color
;;  - 'parenthesis
;;  - 'hash-colon-keyword
;;  - 'symbol
;;  - 'eof
;;  - 'other

;; A Tokens is a [Listof Token]

;; read-token :
;; Any Input-Port Bool Bool -> (U Token Eof)
;; ic? is whether to include comments,
;; iw? is whether to include whitespace
(define (read-token src in [ic? #f] [iw? #f])
  (define-values [ln col pos] (port-next-location in))
  (cond
    [(and ic? (equal? (peek-string 2 0 in) "#|"))
     ; special case for block comments
     (define in* (peeking-input-port in))
     (define-values [_mt _type _paren start end] (racket-lexer in*))
     (define span (and start end (- end start)))
     (define str (read-string span in))
     (token str 'comment (srcloc src ln col pos span))]
    [else
     ; normal behaviour
     (define-values [str type _paren start end] (racket-lexer in))
     (define span (and start end (- end start)))
     (define loc (srcloc src ln col pos span))
     (cond
       [(eof-object? str) eof]
       [(symbol=? type 'eof) eof]
       [(symbol=? type 'white-space)
        (cond [iw?  (token str type loc)]
              [else (read-token src in ic? iw?)])]
       [(symbol=? type 'comment)
        (cond [ic?  (token (format ";~a\n" str) type loc)]
              [else (read-token src in ic? iw?)])]
       [else (token str type loc)])]))

;; read-tokens :
;; Any Input-Port Bool Bool -> Tokens
(define (read-tokens src in [ic? #f] [iw? #f])
  (for/list ([tok (in-port (λ (in) (read-token src in ic? iw?)) in)])
    tok))

;; string->tokens : String Bool Bool -> Tokens
(define (string->tokens str [ic? #f] [iw? #f])
  (parameterize ([port-count-lines-enabled #true])
    (read-tokens 'string (open-input-string str) ic? iw?)))

(module+ test
  (check-equal? (string->tokens "a #|b|# c ")
                (list (token "a" 'symbol (srcloc 'string 1 0 1 1))
                      (token "c" 'symbol (srcloc 'string 1 8 9 1))))
  (check-equal? (string->tokens "a #|b|# c " #t)
                (list (token "a" 'symbol (srcloc 'string 1 0 1 1))
                      (token "#|b|#" 'comment (srcloc 'string 1 2 3 5))
                      (token "c" 'symbol (srcloc 'string 1 8 9 1))))
  (check-equal? (string->tokens
                 (string-append
                  "(define (f x)\n"
                  "  (for/sum ([i (in-range x)])\n"
                  "    (expt i 2)))\n"))
                (list
                 (token "(" 'parenthesis (srcloc 'string 1 0 1 1))
                 (token "define" 'symbol (srcloc 'string 1 1 2 6))
                 (token "(" 'parenthesis (srcloc 'string 1 8 9 1))
                 (token "f" 'symbol (srcloc 'string 1 9 10 1))
                 (token "x" 'symbol (srcloc 'string 1 11 12 1))
                 (token ")" 'parenthesis (srcloc 'string 1 12 13 1))
                 (token "(" 'parenthesis (srcloc 'string 2 2 17 1))
                 (token "for/sum" 'symbol (srcloc 'string 2 3 18 7))
                 (token "(" 'parenthesis (srcloc 'string 2 11 26 1))
                 (token "[" 'parenthesis (srcloc 'string 2 12 27 1))
                 (token "i" 'symbol (srcloc 'string 2 13 28 1))
                 (token "(" 'parenthesis (srcloc 'string 2 15 30 1))
                 (token "in-range" 'symbol (srcloc 'string 2 16 31 8))
                 (token "x" 'symbol (srcloc 'string 2 25 40 1))
                 (token ")" 'parenthesis (srcloc 'string 2 26 41 1))
                 (token "]" 'parenthesis (srcloc 'string 2 27 42 1))
                 (token ")" 'parenthesis (srcloc 'string 2 28 43 1))
                 (token "(" 'parenthesis (srcloc 'string 3 4 49 1))
                 (token "expt" 'symbol (srcloc 'string 3 5 50 4))
                 (token "i" 'symbol (srcloc 'string 3 10 55 1))
                 (token "2" 'constant (srcloc 'string 3 12 57 1))
                 (token ")" 'parenthesis (srcloc 'string 3 13 58 1))
                 (token ")" 'parenthesis (srcloc 'string 3 14 59 1))
                 (token ")" 'parenthesis (srcloc 'string 3 15 60 1)))))

