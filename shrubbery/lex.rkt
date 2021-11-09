#lang racket/base
(require parser-tools/lex
         (for-syntax racket/base)
         (prefix-in : parser-tools/lex-sre)
         "private/property.rkt")

(provide lex/status
         lex-all

         token-name
         ;;  'identifier
         ;;  'literal
         ;;  'comment
         ;;  'whitespace
         ;;
         ;;  'operator
         ;;  'block-operator
         ;;  'continue-operator
         ;;  'bar-operator
         ;;
         ;;  'opener
         ;;  'closer
         ;;  'comma-operator
         ;;  'semicolon-operator
         ;;
         ;;  's-exp
         ;;
         ;;  'EOF
         ;;
         ;;  'fail

         token?
         token-value
         token-e
         token-line
         token-column
         token-srcloc
         token-rename

         syntax->token
         stx-for-original-property

         current-lexer-source

         make-in-text-status
         lex-nested-status?)

(define-lex-abbrevs
  
  ;; For case insensitivity
  [e (char-set "eE")]

  [digit (:/ "0" "9")]
  [digit_ (:or digit (:: digit "_"))]
  [digit16 (:/ "af" "AF" "09")]
  [digit16_ (:or digit16 (:: digit16 "_"))]
  [digit8 (:/ "0" "7")]

  [langchar (:or (:/ "az" "AZ" "09") "+" "-" "_")]

  ;; does not constrain to avoid surrogates:
  [unicode  (:or (:: "u" (:** 1 4 digit16))
                 (:: "U" (:** 1 6 digit16)))]
  
  [str (:: "\"" (:* string-element ) "\"")]

  [string-element (:or (:~ "\"" "\\" "\n" "\r")
                       (:: "\\" unicode)
                       string-escape)]

  [byte-str (:: "#\"" (:* byte-string-element) "\"")]
  [byte-string-element (:or (:- (:/ "\x00" "\xFF") "\"" "\\" "\n" "\r")
                            string-escape)]
  [string-escape (:or "\\\""
                      "\\\\"
                      "\\a"
                      "\\b"
                      "\\t"
                      "\\n"
                      "\\v"
                      "\\f"
                      "\\r"
                      "\\e"
                      "\\'"
                      (:: "\\" (:** 1 3 digit8))
                      (:: "\\x" (:** 1 2 digit16)))]

  [bad-str (:: (:? "#") "\"" 
               (:* (:~ "\"" "\\")
                   (:: "\\" any-char))
               (:? "\\" "\""))]

  [boolean (:or "#true" "#false")]
                      
  [special-number (:: "#"
                      (:or "inf"
                           "neginf"
                           "nan"))]

  [bad-hash (:- (:or (:: "#" (:* non-delims))
                     "#/")
                boolean
                special-number)]

  [exponent-marker e]
  [sign (char-set "+-")]
  
  [script (:: "#!" (:or #\space #\/) (:* (:~ #\newline) (:: #\\ #\newline)))]
  
  [identifier (:: (:or alphabetic "_")
                  (:* (:or alphabetic numeric "_")))]
  [opchar (:or (:- symbolic (:or "~"))
               (:- punctuation (:or "," ";" "#" "\\" "_" "@" "\""
                                    "(" ")" "[" "]" "{" "}" "«" "»")))]
  [operator (:- (:or opchar
                     (:: (:* opchar) (:- opchar "+" "-" "." "/"))
                     (:+ ".")
                     (:+ "+")
                     (:+ "-"))
                "|" ":"
                (:: (:* any-char) (:or "//" "/*") (:* any-char)))]

  [keyword (:: "~" identifier)]
  [bad-keyword (:: "~")]
  
  ;; disallows a number that starts +, -, or "."
  [number/continuing (:or decimal-number/continuing
                          hex-number)]
  [number (:: (:? sign)
              (:or decimal-number
                   hex-number))]
  
  [uinteger (:: (:* digit_) digit)]
  [uinteger16 (:: (:* digit16_) digit16)]
  
  [decimal-number/continuing (:or (:: uinteger number-exponent)
                                  (:: uinteger "." (:? uinteger) number-exponent))]
  [decimal-number (:or decimal-number/continuing
                       (:: "." uinteger number-exponent))]
  [number-exponent (:or "" (:: exponent-marker (:? sign) uinteger))]
  [hex-number (:: "0x" uinteger16)]

  [bad-number/continuing (:- (:: digit (:+ non-number-delims))
                             identifier
                             number/continuing)]
  [bad-number (:- (:: (:? sign) digit (:+ non-number-delims))
                  identifier
                  number)]
  [bad-comment "*/"]

  [non-number-delims (:or non-delims ".")]
  [non-delims (:or alphabetic numeric "_")]

  ;; making whitespace end at newlines is for interactive parsing
  ;; where we end at a blank line
  [whitespace-segment (:or (:+ (:- whitespace "\n"))
                           (:: (:* (:- whitespace "\n")) "\n"))])

(define-syntax (ret stx)
  (syntax-case stx (quote)
    [(_ (quote name) lexeme #:raw raw (quote type) more ...)
     (with-syntax ([ht (hasheq 'type #'type 'rhombus-type #'name)])
       #`(make-ret (quote name) lexeme #:raw raw ht more ...))]
    [(_ name lexeme type more ...)
     #`(ret name lexeme #:raw #f type more ...)]))

(define (make-ret name lexeme #:raw [raw #f] attribs paren start-pos end-pos status)
  (define backup 0)
  (values (make-token name lexeme start-pos end-pos raw)
          attribs paren (position-offset start-pos) (position-offset end-pos)
          backup status))

(define stx-for-original-property (read-syntax #f (open-input-string "original")))
(define current-lexer-source (make-parameter "input"))

(define (make-token name e start-pos end-pos [raw #f])
  (define offset (position-offset start-pos))
  (define loc (vector (current-lexer-source)
                      (position-line start-pos)
                      (position-col start-pos)
                      offset
                      (- (position-offset end-pos)
                         offset)))
  (token name (let loop ([e e] [raw raw])
                (let ([e (if (pair? e)
                             (let p-loop ([e e])
                               (cond
                                 [(null? (cdr e)) (list (loop (car e) raw))]
                                 [else (cons (loop (car e) #f)
                                             (p-loop (cdr e)))]))
                             e)]
                      [raw (if (pair? e) #f raw)])
                  (define stx (datum->syntax #f
                                             e
                                             loc
                                             stx-for-original-property))
                  (if (eq? name 'comment)
                      stx
                      (syntax-raw-property stx (or raw (if (string? e) e '()))))))))

(define (read-line-comment name lexeme input-port start-pos
                           #:status [status 'initial]
                           #:consume-newline? [consume-newline? #f])
  (let ([comment (apply string (append (string->list lexeme) (read-line/skip-over-specials input-port
                                                                                           consume-newline?)))])
    (define-values (end-line end-col end-offset) (port-next-location input-port))
    (values (make-token name comment start-pos (position end-offset end-line end-col))
            'comment #f 
            (position-offset start-pos)
            end-offset
            0
            status)))

(define get-next-comment
  (lexer
   ["/*" (values 1 end-pos lexeme)]
   ["*/" (values -1 end-pos lexeme)]
   [(:or "/" "*" (:* (:~ "*" "/")))
    (let-values ([(delta end-pos rest-lexeme) (get-next-comment input-port)])
      (values delta end-pos (string-append lexeme rest-lexeme)))]
   [(eof) (values 'eof end-pos "")]
   [(special)
    (get-next-comment input-port)]
   [(special-comment)
    (get-next-comment input-port)]))

(define (read-nested-comment num-opens start-pos lexeme input)
  (define-values (diff end next-lexeme) (get-next-comment input))
  (cond
    [(eq? 'eof diff) (ret 'fail eof 'error #f start-pos end 'initial)]
    [else
     (define all-lexeme (string-append lexeme next-lexeme))
     (define next-num-opens (+ diff num-opens))
     (cond
       [(= 0 next-num-opens) (ret 'comment all-lexeme 'comment #f start-pos end 'initial)]
       [else (read-nested-comment next-num-opens start-pos all-lexeme input)])]))

(define (get-offset i)
  (let-values (((x y offset) (port-next-location i)))
    offset))

(define (read-line/skip-over-specials i consume-newline?)
  (let loop ()
    (define next (peek-char-or-special i))
    (cond
      [(eq? next #\newline)
       (cond
         [consume-newline?
          (read-char-or-special i)
          (list #\newline)]
         [else null])]
      [(eof-object? next)
       null]
      [else
       (read-char-or-special i)
       (if (char? next)
           (cons next (loop))
           (loop))])))

(struct s-exp-mode (depth status) #:prefab)
(struct in-at (mode comment? closeable? opener shrubbery-status openers) #:prefab)
(struct in-escaped (shrubbery-status at-status) #:prefab)

(define (make-in-text-status)
  (in-at 'inside #f #f "" 'initial 0))

(define (lex-nested-status? status)
  (not (or (not status) (symbol? status))))

(define (lex/status in pos status racket-lexer*/status)
  (let-values ([(tok type paren start end backup status)
                (let loop ([status status])
                  (cond
                    [(s-exp-mode? status)
                     ;; within `#{}`
                     (unless racket-lexer*/status
                       (error "shouldn't be in S-expression mode without a Racket lexer"))
                     (define depth (s-exp-mode-depth status))
                     (cond
                       [(and (zero? depth)
                             (eqv? #\} (peek-char in)))
                        ;; go out of S-expression mode by using shrubbery lexer again
                        (shrubbery-lexer/status in)]
                       [else
                        (define-values (tok type paren start end backup s-exp-status action)
                          (racket-lexer*/status in pos (s-exp-mode-status status)))
                        (values tok type paren start end backup (case action
                                                                  [(open)
                                                                   (s-exp-mode (add1 depth) s-exp-status)]
                                                                  [(close)
                                                                   (s-exp-mode (sub1 depth) s-exp-status)]
                                                                  [else
                                                                   (s-exp-mode depth s-exp-status)]))])]
                    [(in-at? status)
                     ;; within an `@` sequence
                     (define-values (tok type paren start end backup new-status)
                       (at-lexer in status (lambda (status) (loop status))))
                     (define new-type (if (and (in-at-comment? status)
                                               (not (eq? type 'eof)))
                                          (hash-set (if (hash? type) type (hash 'type type)) 'comment? #t)
                                          type))
                     (values tok new-type paren start end backup new-status)]
                    [(in-escaped? status)
                     (define-values (t type paren start end backup sub-status)
                       (loop (in-escaped-shrubbery-status status)))
                     (values t type paren start end backup (struct-copy in-escaped status
                                                                        [shrubbery-status sub-status]))]
                    [(eq? status 'continuing)
                     ;; normal mode, after a form
                     (shrubbery-lexer-continuing/status in)]
                    [else
                     ;; normal mode, at start or after an operator or whitespace
                     (shrubbery-lexer/status in)]))])
    (cond
      [(and (token? tok)
            (eq? (token-name tok) 'at-content)
            (eqv? 0 (string-length (token-e tok))))
       ;; a syntax coloring lexer must not return a token that
       ;; consumes no characters, so just drop it by recurring
       (lex/status in pos status racket-lexer*/status)]
      [else
       (define new-backup (cond
                            ;; If we have "@/{" and we add a "/" after the existing one,
                            ;; we'll need to back up more:
                            [(not (token? tok)) backup]
                            [(eq? (token-name tok) 'at-opener) 1]
                            [(eq? (token-name tok) 'at-comment) 3]
                            [(and (in-at? status) (eq? (token-name tok) 'operator)) 2]
                            [else backup]))
       (values tok type paren start end new-backup status)])))

(define-syntax-rule (make-lexer/status number bad-number)
  (lexer
   [whitespace-segment
    (ret 'whitespace lexeme 'white-space #f start-pos end-pos 'initial)]
   [str (ret 'literal (parse-string lexeme) #:raw lexeme 'string #f start-pos end-pos 'datum)]
   [byte-str (ret 'literal (parse-byte-string lexeme) #:raw lexeme 'string #f start-pos end-pos 'datum)]
   [bad-number
    (ret 'fail lexeme 'error #f start-pos end-pos 'continuing)]
   [number
    (ret 'literal (parse-number lexeme) #:raw lexeme 'constant #f start-pos end-pos 'continuing)]
   [special-number
    (let ([num (case lexeme
                 [("#inf") +inf.0]
                 [("#neginf") -inf.0]
                 [("#nan") +nan.0])])
      (ret 'literal num #:raw lexeme 'constant #f start-pos end-pos 'continuing))]
   [boolean
    (ret 'literal (equal? lexeme "#true") #:raw lexeme 'constant #f start-pos end-pos 'continuing)]
   ["//" (read-line-comment 'comment lexeme input-port start-pos)]
   ["/*" (read-nested-comment 1 start-pos lexeme input-port)]
   ["#//"
    (ret 'group-comment lexeme 'comment #f start-pos end-pos 'initial)]
   [(:: (:or "#lang " "#!")
        (:or langchar
             (:: langchar (:* (:or langchar "/")) langchar)))
    (ret 'comment lexeme 'other #f start-pos end-pos 'initial)]
   [(:: (:or "#lang " "#!") (:* (:& any-char (complement whitespace))))
    (ret 'fail lexeme 'error #f start-pos end-pos 'initial)]
   [script
    (ret 'comment lexeme 'comment #f start-pos end-pos 'initial)]
   [(:or "(" "[" "{" "«")
    (ret 'opener lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos 'initial)]
   [(:or ")" "]" "}" "»")
    (ret 'closer lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos 'continuing)]
   ["#{"
    (ret 's-exp lexeme 'parenthesis '|{| start-pos end-pos (s-exp-mode 0 #f))]
   [":"
    (ret 'block-operator lexeme 'block-operator #f start-pos end-pos 'initial)]
   ["|"
    (ret 'bar-operator lexeme 'bar-operator #f start-pos end-pos 'initial)]
   ["\\"
    (ret 'continue-operator lexeme 'continue-operator #f start-pos end-pos 'initial)]
   [","
    (ret 'comma-operator lexeme 'separator #f start-pos end-pos 'initial)]
   [";"
    (ret 'semicolon-operator lexeme 'separator #f start-pos end-pos 'initial)]
   [identifier
    (ret 'identifier (string->symbol lexeme) #:raw lexeme 'symbol #f start-pos end-pos 'continuing)]
   [operator
    (ret 'operator (list 'op (string->symbol lexeme)) #:raw lexeme 'operator #f start-pos end-pos 'initial)]
   [keyword
    (let ([kw (string->keyword (substring lexeme 1))])
      (ret 'identifier kw #:raw lexeme 'hash-colon-keyword #f start-pos end-pos 'continuing))]
   ["@//"
    (let ([opener (peek-at-opener input-port)])
      (if opener
          (ret 'at-comment lexeme 'comment (string->symbol lexeme) start-pos end-pos (in-at 'open #t #t opener 'initial '()))
          (read-line-comment 'at-comment lexeme input-port start-pos)))]
   ["@"
    (let ([opener (peek-at-opener input-port)])
      (define mode (if opener 'open 'initial))
      (ret 'at lexeme 'at (string->symbol lexeme) start-pos end-pos (in-at mode #f #t opener 'initial '())))]
   [(special)
    (cond
      [(or (number? lexeme) (boolean? lexeme))
       (ret 'literal lexeme 'constant #f start-pos end-pos 'continuing)]
      [(string? lexeme)
       (ret 'literal lexeme 'string #f start-pos end-pos 'continuing)]
      [(keyword? lexeme)
       (ret 'literal lexeme 'hash-colon-keyword #f start-pos end-pos 'continuing)]
      [else
       (ret 'literal lexeme 'no-color #f start-pos end-pos 'continuing)])]
   [(special-comment)
    (ret 'comment "" 'comment #f start-pos end-pos 'initial)]
   [(eof) (ret-eof start-pos end-pos)]
   [(:or bad-str bad-keyword bad-hash bad-comment)
    (ret 'fail lexeme 'error #f start-pos end-pos 'bad)]
   [any-char (extend-error lexeme start-pos end-pos input-port)]))

(define (ret-eof start-pos end-pos)
  (values (make-token 'EOF eof start-pos end-pos) 'eof #f #f #f 0 #f))

(define shrubbery-lexer/status (make-lexer/status number bad-number))
(define shrubbery-lexer-continuing/status (make-lexer/status number/continuing bad-number/continuing))

;; after reading `@`, we enter an at-exp state machine for whether
;; we're in the initial part, within `[]`, or within `{}`; we have to
;; perform some parsing here to balance openers and closers; we leave
;; wehite trimming to the parser layer
(define (at-lexer in status recur)
  (define in-mode (in-at-mode status))
  (define (get-expected opener/ch ch/closer)
    (define (get-all-expected s)
      (for ([ch (in-string s)])
        (unless (eqv? ch (read-char in))
          (error "inconsistent input" ch))))
    (define start-pos (next-location-as-pos in))
    (define eof?
      (cond
        [(string? opener/ch)
         (get-all-expected opener/ch)
         (unless (eqv? ch/closer (read-char in))
           (error "inconsistent opener input" ch/closer))
         #f]
        [else
         (define ch (read-char in))
         (cond
           [(eof-object? ch) #t]
           [else
            (unless (eqv? opener/ch ch)
              (error "inconsistent closer input" opener/ch))
            (get-all-expected ch/closer)
            #f])]))
    (define end-pos (next-location-as-pos in))
    (values start-pos end-pos eof?))
  (case in-mode
    ;; 'initial mode is right after `@` without immediate `{`, and we
    ;; may transition from 'initial mode to 'brackets mode at `[`
    [(initial brackets)
     ;; recur to parse in shrubbery mode:
     (define-values (t type paren start end backup sub-status)
       (recur (in-at-shrubbery-status status)))
     ;; to keep the term and possibly exit 'initial or 'brackets mode:
     (define (ok status)
       (values t type paren start end 0 (cond
                                          [(and (not (s-exp-mode? sub-status))
                                                (null? (in-at-openers status)))
                                           ;; either `{`, `[`, or back to shrubbery mode
                                           (define opener (peek-at-opener in))
                                           (cond
                                             [opener
                                              (in-at 'open (in-at-comment? status) #t opener sub-status '())]
                                             [(and (not (eq? in-mode 'brackets))
                                                   (eqv? #\[ (peek-char in)))
                                              (in-at 'brackets (in-at-comment? status) #t #f sub-status '())]
                                             [(in-escaped? sub-status)
                                              (in-escaped-at-status sub-status)]
                                             [else sub-status])]
                                          [else
                                           ;; continue in-at mode
                                           status])))
     ;; converts a token to an error token:
     (define (error status)
       (values (struct-copy token t [name 'fail]) 'error #f start end 0 status))
     ;; update the shrubbery-level status, then keep the term or error,
     ;; trackig nesting depth through the status as we continue:
     (let ([status (struct-copy in-at status
                                [shrubbery-status sub-status])])
       (case (token-name t)
         [(opener) (ok (struct-copy in-at status
                                    [openers (cons (token-e t)
                                                   (in-at-openers status))]))]
         [(closer)
          (cond
            [(and (pair? (in-at-openers status))
                  (closer-for? (token-e t) (car (in-at-openers status))))
             (ok (struct-copy in-at status
                              [openers (cdr (in-at-openers status))]))]
            [else
             (error status)])]
         [else (ok status)]))]
    ;; 'open mode is right after `@` when the next character is `{`,
    ;; or after a closing `}` when the next character is `{`
    [(open)
     (define opener (in-at-opener status))
     (define-values (start-pos end-pos eof?) (get-expected opener #\{))
     (ret 'at-opener (string-append opener "{") 'parenthesis '|{| start-pos end-pos
          (struct-copy in-at status [mode 'inside] [openers 0]))]
    ;; 'inside mode means in `{}` and not currently escaped, and we
    ;; transition to 'escape mode on a `@`, and we transition to 'close mode
    ;; on a `}` that is not balancing a `{` within `{}`
    [(inside)
     (define opener (in-at-opener status))
     (define closeable? (in-at-closeable? status))
     (define start-pos (next-location-as-pos in))
     (define o (open-output-string))
     (let loop ([depth (in-at-openers status)])
       (define ch (peek-char in))
       (cond
         [(eqv? ch #\newline)
          ;; convert a newline into a separate string input
          (define s (get-output-string o))
          (cond
            [(= 0 (string-length s))
             (read-char in)
             (define end-pos (next-location-as-pos in))
             (ret 'at-content "\n" 'text #f start-pos end-pos
                  (struct-copy in-at status [mode 'inside] [openers depth]))]
            [else
             (define end-pos (next-location-as-pos in))
             (ret 'at-content s 'text #f start-pos end-pos
                  (struct-copy in-at status [mode 'inside] [openers depth]))])]
         [(or (eof-object? ch)
              (and closeable?
                   (peek-at-closer in #:opener opener)))
          (cond
            [(zero? depth)
             ;; `lex/status` will handle the case that the content is empty
             (define end-pos (next-location-as-pos in))
             (ret 'at-content (get-output-string o) 'text #f start-pos end-pos
                  (struct-copy in-at status [mode 'close]))]
            [else
             (if (equal? opener "")
                 (write-char (read-char in) o)
                 (write-string (read-string (add1 (bytes-length opener)) in) o))
             (loop (sub1 depth))])]
         [(peek-at-prefixed #\@ in #:opener opener)
          ;; `lex/status` will handle the case that the content is empty
          (define end-pos (next-location-as-pos in))
          (ret 'at-content (get-output-string o) 'text #f start-pos end-pos
               (struct-copy in-at status [mode 'escape] [openers depth]))]
         [(and closeable?
               (peek-at-opener in #:opener opener))
          (if (equal? opener "")
              (write-char (read-char in) o)
              (write-string (read-string (add1 (bytes-length opener)) in) o))
          (loop (add1 depth))]
         [else
          (write-char (read-char in) o)
          (loop depth)]))]
    ;; 'escape mode means in `{}`, not currently escaped, and expect `@` next
    [(escape)
     (define opener (in-at-opener status))
     (define-values (start-pos end-pos eof?) (get-expected opener #\@))
     (cond
       [(read-at-comment in)
        => (lambda (slashes)
             (cond
               [(peek-at-opener in)
                => (lambda (opener)
                     ;; block comment
                     (define end-pos (next-location-as-pos in))
                     (ret 'at-comment (string-append opener "@" slashes) 'comment #f start-pos end-pos
                          (in-at 'open #t #t opener (in-escaped 'initial (struct-copy in-at status [mode 'inside])) '())))]
               [else
                ;; line comment
                (read-line-comment 'comment (string-append opener "@" slashes) in start-pos
                                   #:status (struct-copy in-at status [mode 'inside])
                                   #:consume-newline? #t)]))]
       [else
        (define next-opener (peek-at-opener in))
        (define mode (if next-opener 'open 'initial))
        (ret 'at (string-append opener "@") 'at #f start-pos end-pos
             (in-at mode (in-at-comment? status) #t next-opener (in-escaped 'initial (struct-copy in-at status [mode 'inside])) '()))])]
    ;; 'close mode handles the final `}` of a `{}`
    [(close)
     (define closer (at-opener->closer (in-at-opener status)))
     (define-values (start-pos end-pos eof?) (get-expected #\} closer))
     (cond
       [eof? (ret-eof start-pos end-pos)]
       [else
        (define sub-status (in-at-shrubbery-status status))
        ;; might continue with another immediate opener:
        (define next-opener (peek-at-opener in))
        (ret 'at-closer (string-append "}" closer) 'parenthesis '|}| start-pos end-pos
             (if next-opener
                 (in-at 'open (in-at-comment? status) #t next-opener sub-status '())
                 (if (in-escaped? sub-status)
                     (in-escaped-at-status sub-status)
                     sub-status)))])]
    [else (error "unknown at-exp state")]))

(define (peek-at-opener in #:opener [opener #f])
  (cond
    [opener
     ;; look for another instance of the current opener
     (peek-at-prefixed #\{ in #:opener opener)]
    [else
     ;; look for a fresh opener
     (define ch (peek-char in))
     (cond
       [(eqv? ch #\{) ""]
       [(eqv? ch #\|)
        (let loop ([chars '(#\|)] [offset 1])
          (define ch (peek-char in offset))
          (cond
            [(eqv? ch #\{) (list->string (reverse chars))]
            [(and ((char->integer ch) . < . 128)
                  (or (char-symbolic? ch)
                      (char-punctuation? ch)))
             (loop (cons ch chars) (add1 offset))]
            [else #f]))]
       [else #f])]))

(define (peek-at-prefixed ch in #:opener opener)
  (let loop ([offset 0])
    (cond
      [(= offset (string-length opener))
       (if (eqv? ch (peek-char in offset))
           opener
           #f)]
      [(eqv? (peek-char in offset) (string-ref opener offset))
       (loop (add1 offset))]
      [else #f])))

(define (peek-at-closer in #:opener [opener #f])
  (define ch (peek-char in))
  (cond
    [(eqv? ch #\})
     (let loop ([offset 0])
       (cond
         [(= offset (string-length opener)) opener]
         [(eqv? (peek-char in (add1 offset))
                (flip-at-bracket (string-ref opener (- (string-length opener) offset 1))))
          (loop (add1 offset))]
         [else #f]))]
    [else #f]))

(define (read-at-comment in)
  (and (eqv? (peek-char in) #\/)
       (eqv? (peek-char in 1) #\/)
       (begin
         (read-char in)
         (read-char in)
         "//")))

(define (flip-at-bracket ch)
  (case ch
    [(#\<) #\>]
    [(#\>) #\<]
    [(#\[) #\]]
    [(#\]) #\[]
    [(#\() #\)]
    [(#\)) #\(]
    [else ch]))

(define (at-opener->closer opener)
  (cond
    [(eqv? 0 (string-length opener)) ""]
    [else
     (list->string (reverse (for/list ([ch (in-string opener)])
                              (flip-at-bracket ch))))]))

(define (next-location-as-pos in)
  (define-values (line col pos) (port-next-location in))
  (position pos line col))
  
(define (extend-error lexeme start end in)
  (define next (peek-char-or-special in))
  (if (or (memq next
                `(special
                  #\" #\, #\' #\` #\( #\) #\[ #\] #\{ #\} #\;
                  ,eof))
          (char-whitespace? next))
      (ret 'fail lexeme 'error #f start end 'bad)
      (let-values (((rest end-pos) (get-chunk in)))
        (ret 'fail (string-append lexeme rest) 'error #f start end-pos 'bad))))

(define get-chunk
  (lexer
   [(:+ whitespace) (values lexeme end-pos)]))

(define (parse-number s)
  (if (and ((string-length s) . > . 2)
           (eqv? #\x (string-ref s 1)))
      (string->number (regexp-replace* #rx"_" (substring s 2) "") 16)
      (string->number (regexp-replace* #rx"_" s ""))))

(define (parse-string s)
  (read (open-input-string s)))

(define (parse-byte-string s)
  (read (open-input-string s)))

(define (parse-char s)
  (define str
    (read (open-input-string (string-append "\""
                                            (substring s 1 (sub1 (string-length s)))
                                            "\""))))
  (string-ref str 0))

(struct token (name value))
(struct located-token token (srcloc))

(define (token-e t)
  (syntax-e (token-value t)))

(define (token-line t)
  (if (located-token? t)
      (srcloc-line (located-token-srcloc t))
      (syntax-line (token-value t))))

(define (token-column t)
  (let ([c (if (located-token? t)
               (srcloc-column (located-token-srcloc t))
               (syntax-column (token-value t)))])
    (if (and c (eq? (token-name t) 'bar-operator))
        (+ c 0.5)
        c)))

(define (token-srcloc t)
  (cond
    [(located-token? t)
     (located-token-srcloc t)]
    [else
     (define s (token-value t))
     (srcloc (syntax-source s)
             (syntax-line s)
             (syntax-column s)
             (syntax-position s)
             (syntax-span s))]))

(define (token-rename t name)
  (struct-copy token t [name name]))

(define (syntax->token name s [srcloc #f])
  (if srcloc
      (located-token name s srcloc)
      (token name s)))

;; Runs `lex/status` in a loop, but switches to `finish-s-exp`
;; for an S-expression escape:
(define (lex-all in fail
                 #:text-mode? [text-mode? #f]
                 #:keep-type? [keep-type? #f]
                 #:source [source (object-name in)]
                 #:interactive? [interactive? #f])
  (define status (if text-mode?
                     (make-in-text-status)
                     'initial))
  (parameterize ([current-lexer-source source])
    (let loop ([status status] [depth 0] [blanks 0] [multi? #f])
      (cond
        [(eof-object? (peek-char in))
         ;; don't consume an EOF
         '()]
        [else
         (define-values (tok type paren start-pos end-pos backup new-status)
           (lex/status in (file-position in) status #f))
         (define (wrap r)
           (if keep-type?
               (vector r type paren)
               r))
         (define name (token-name tok))
         (case name
           [(EOF) '()]
           [(fail) (fail tok "read error")]
           [(whitespace)
            (define a (wrap tok))
            (define newline? (let* ([s (syntax-e (token-value tok))]
                                    [len (string-length s)])
                               (and (positive? len)
                                    (eqv? #\newline (string-ref s (sub1 len))))))
            (cond
              [(and interactive? newline? (zero? depth)
                    (blanks . >= . (if multi? 1 0))
                    (not (lex-nested-status? status)))
               (list (wrap tok))]
              [else (cons (wrap tok)
                          (loop new-status depth (+ blanks (if newline? 1 0)) multi?))])]
           [else
            (define a (case name
                        [(s-exp)
                         (wrap (finish-s-exp tok in fail))]
                        [else (wrap tok)]))
            (define d (loop (case name
                              [(s-exp) 'continuing]
                              [else new-status])
                            (case name
                              [(opener) (add1 depth)]
                              [(closer) (sub1 depth)]
                              [else depth])
                            0
                            (case name
                              [(block-operator semicolon-operator)
                               (or multi?
                                   (and (zero? depth) (not (lex-nested-status? status))))]
                              [else multi?])))
            (cons a d)])]))))

(define (finish-s-exp open-tok in fail)
  (define v (read-syntax (current-lexer-source) in))
  (when (eof-object? v)
    (fail open-tok "expected S-expression after `#{`"))
  (define end-pos
    (let loop ()
      (define-values (line col pos) (port-next-location in))
      (define c (read-char in))
      (cond
        [(eof-object? c)
         (fail v "expected `}` after S-expression")]
        [(eqv? c #\})
         (add1 pos)]
        [(char-whitespace? c)
         (loop)]
        [else
         (define bad (datum->syntax #f c (list (current-lexer-source)
                                               line
                                               col
                                               pos
                                               1)))
         (fail bad "expected only whitespace or `}` after S-expression")])))
  (define result
    (syntax->token (if (identifier? v) 'identifier 'literal)
                   v
                   (let ([loc (token-srcloc open-tok)])
                     (struct-copy srcloc loc
                                  [span (- end-pos (srcloc-position loc))]))))
  (when (pair? (syntax-e v))
    (fail result "S-expression in `#{` and `}` must not be a pair"))
  result)

(define (closer-for? cl op)
  (equal? cl (case op
               [("(") ")"]
               [("[") "]"]
               [("{") "}"]
               [("«") "»"]
               [else #f])))

(define (consume-only-whitespace-line? in)
  (define (consume)
    (read-line in)
    #t)
  (let loop ([i 0])
    (define ch (peek-char in i))
    (cond
      [(eof-object? ch) (consume)]
      [(or (eqv? ch #\newline)
           (eqv? ch #\return))
       (consume)]
      [(char-whitespace? ch)
       (loop (+ i (char-utf-8-length ch)))]
      [else #f])))
