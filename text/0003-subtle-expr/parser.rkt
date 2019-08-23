#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         racket/list
         racket/runtime-path
         racket/path
         racket/file
         racket/match
         syntax/parse/define
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         syntax/readerr)

(define-tokens data (ATOM))
(define-empty-tokens delim (HASH-COMMENT WS OP CP OBK CBK OBS CBS OPK CPK AT COLON COMMA DOT INDENT OUTDENT NL EOF))

(define (get-position ip)
  (define-values (line col offset) (port-next-location ip))
  (position offset line col))

(define (make-subtle-lexer ip)
  (let ()
    (define-lex-abbrevs
      [digit (:/ #\0 #\9)]
      [atom-char (:~ (char-set "\n ()[]{}<>@:,.;#"))]
      [comment
       (:or (:: "//" (:* (:~ #\newline)) #\newline)
            (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))]
      
      [num10 (:: (:+ digit) (:or "" (:: #\. (:+ digit))))])

    (define get-string-token
      (lexer
       [(:~ #\" #\\) (cons (car (string->list lexeme))
                           (get-string-token input-port))]
       [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
       [(:: #\\ #\n) (cons #\newline (get-string-token input-port))]
       [(:: #\\ #\") (cons #\" (get-string-token input-port))]
       [#\" null]))

    (define core-lexer
      (lexer-src-pos
       ["#;" (token-HASH-COMMENT)] ;; ""
       [#\space 'WS]
       [#\( 'OP] [#\) 'CP]
       [#\[ 'OBK] [#\] 'CBK]
       [#\{ 'OBS] [#\} 'CBS]
       [#\< 'OPK] [#\> 'CPK]
       [#\@ 'AT] [#\, 'COMMA] [#\. 'DOT]
       [(:: #\: #\newline)
        (begin (set! full-indent (add1 full-indent))
               (set! expect-indent? #t)
               'INDENT)]
       [#\: 'COLON]
       [#\newline
        (begin (set! expect-indent? (and (not (zero? full-indent)) #t))
               'NL)]
       [comment (return-without-pos (core-lexer input-port))]
       [num10 (token-ATOM (string->number lexeme 10))]
       [(eof) 'EOF]
       [#\" (token-ATOM (list->string (get-string-token input-port)))]
       [(:: atom-char (:* atom-char)) (token-ATOM (string->symbol lexeme))]))

    (define previous '())
    (define full-indent 0)
    (define expect-indent? #f)
    (define (indent-lexer ip)
      (cond
        [(not (null? previous))
         (begin0 (car previous)
           (set! previous (cdr previous)))]
        [(not expect-indent?)
         (core-lexer ip)]
        [else
         (define start-pos (get-position ip))
         (define ws (regexp-try-match #rx"^ *" ip))
         (define n (quotient (bytes-length (car ws)) 2))
         (cond
           [(= n full-indent)
            (set! expect-indent? #f)
            (core-lexer ip)]
           [else
            (define end-pos (get-position ip))
            (define OUTDENT (make-position-token 'OUTDENT start-pos end-pos))
            (set! previous (make-list (- full-indent n) OUTDENT))
            (set! full-indent n)
            (set! expect-indent? #f)
            (indent-lexer ip)])]))

    (define current-lexer indent-lexer)

    (λ ()
      (define t (current-lexer ip))
      (eprintf "~a\n" (position-token-token t))
      t)))

(define orig-prop-stx (read-syntax #f (open-input-string "original")))

(define (subtle-parser source-name)
  (define (make-srcloc start-pos end-pos)
    (list source-name
          (position-line start-pos)
          (position-col start-pos)
          (position-offset start-pos)
          (- (position-offset end-pos)
             (position-offset start-pos))))
  (define-simple-macro (build-so start end vals ...)
    #:with start-pos (format-id #'start "$~a-start-pos" (syntax-e #'start))
    #:with end-pos (format-id #'end "$~a-end-pos" (syntax-e #'end))
    (build-so* (list vals ...) (make-srcloc start-pos end-pos)))
  (define (build-so* vals loc)
    (match vals
      [(list '#%cons a '#%null) (list a)]
      [(list '#%cons a d) (cons a d)]
      [(list '#%atom a) a]
      [(list '#%nl-cons '#%null d) (build-so* d loc)]
      [(list '#%nl-cons a d) (build-so* (list '#%cons a d) loc)]
      [_ vals
       #;
       (datum->syntax #f vals loc orig-prop-stx)]))

  (define (snoc l x) (append l (list x)))

  (parser
   (src-pos)

   (start s)
   (end EOF)
   (error
    (lambda (stack tok-ok? name val start-pos end-pos)
      (define stack-states (map car stack))
      (define key (cons stack-states name))
      (define loc (make-srcloc start-pos end-pos))
      (cond
        [(harvesting-errors?) => (λ (esc) (esc key))]
        [(hash-ref error-database key #f) => (λ (m) (show-error m name val loc))]
        [else
         (apply raise-read-error (format "read-error: ~v ~v ~v ~v" stack-states tok-ok? name val) loc)])))
   (tokens data delim)

   (grammar
    (s [(nl-seq) $1])

    (seq [() '#%null]
         [(ne-seq) $1])
    (ne-seq [(one) (build-so 1 1 '#%cons $1 '#%null)]
            [(one COMMA WS ne-seq)
             (build-so 1 4 '#%cons $1
                       (build-so 2 4 '#%cons '#%comma $4))]
            [(one WS ne-seq) (build-so 1 3 '#%cons $1 $3)]
            [(one INDENT nl-seq OUTDENT seq)             
             (build-so 1 5 '#%cons $1
                       (build-so 2 5 '#%cons
                                 (build-so 2 4 '#%indent $3)
                                 $5))])
    (nl-seq [() '#%null]
            [(seq) (build-so 1 1 '#%nl-cons $1 '#%null)]
            [(seq NL nl-seq) (build-so 1 3 '#%nl-cons $1 $3)])

    (one [(atom) (build-so 1 1 '#%atom $1)]
         [(one DOT atom) (build-so 1 3 '#%dot $1 $3)]
         [(OP seq CP) $2]
         [(OBK seq CBK) (build-so 1 3 '#%group #\[ $2)]
         [(OBS seq CBS) (build-so 1 3 '#%group #\{ $2)]
         [(one OP seq CP) (build-so 1 4 '#%app  #\( $1 $3)]
         [(one OBK seq CBK) (build-so 1 3 '#%app #\[ $1 $3)]
         [(one OPK seq CPK) (build-so 1 3 '#%app #\< $1 $3)]
         [(HASH-COMMENT one WS one) $4]
         [(AT one OBS text CBS) (build-so 1 5 '#%txt $2 $4)])

    (atom [(ATOM) $1]
          [(OPK) '<]
          [(CPK) '>]
          [(COLON) ':]
          [(COMMA) '|,|])

    (text [() '()]
          [(text-line) $1]
          [(text-line NL text) (build-so 1 2 '#%txt-seq $1 $3)])
    (text-line [() '()]
               [(text-atom text-line) (build-so 1 2 '#%txt-line $1 $2)])
    (text-atom [(atom) $1]
               [(AT one) (build-so 1 2 '#%esc $2)]))))

(define (parse ip)
  (port-count-lines! ip)
  ;; XXX Need to post-process for precedence and flattening
  ((subtle-parser (object-name ip)) (make-subtle-lexer ip)))

(define (parse-file f)
  (call-with-input-file f parse))

(define harvesting-errors? (make-parameter #f))
(define error-database (make-hash))
(define-runtime-path error-db "error.db")
;; This should be cached or otherwise computed at compile-time
(let ()
  (for ([f (in-directory error-db)]
        #:when (equal? #".rkt" (path-get-extension f)))
    (define val
      (cons f (file->string (path-replace-extension f #".msg"))))
    (define key
      (let/ec esc
        (parameterize ([harvesting-errors? esc])
          (parse-file f))
        (error 'error-database "File ~a did not error!" f)))
    (cond
      [(hash-ref error-database key #f)
       => (λ (val)
            (error 'error-database "File ~a duplicates error of ~a" f (car val)))]
      [else (hash-set! error-database key val)])))
(define (show-error val tok-name tok-val loc)
  (match-define (cons _ msg) val)
  (apply raise-read-error msg loc))

(module+ test
  (define-runtime-path x "x.rkt")
  (define-runtime-path t "t.rkt")
  (define-runtime-path md "../0003-subtle-expr.md")
  (define (seek-subtle l)
    (match l
      ['() (error 'seek-subtle "Subtle block did not start!")]
      [(cons "```subtle" tail) tail]
      [(cons _ l) (seek-subtle l)]))
  (define (extract-subtle l)
    (match l
      ['() (error 'extract-subtle "Subtle block did not end!")]
      [(cons "```" _) '()]
      [(cons x l) (cons x (extract-subtle l))]))
  (display-lines-to-file (extract-subtle (seek-subtle (file->lines md)))
                         t #:exists 'replace)
  (require racket/pretty)
  (pretty-print
   (parse-file x)))
