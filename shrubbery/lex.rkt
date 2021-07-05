#lang racket/base

(require parser-tools/lex
         racket/contract
         (prefix-in : parser-tools/lex-sre))

(provide lex/status

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
         
         token-value
         token-e
         token-line
         token-column
         token-srcloc

         syntax->token

         current-lexer-source)

(define-lex-abbrevs
  
  ;; For case insensitivity
  [a (char-set "aA")]
  [b (char-set "bB")]
  [c (char-set "cC")]
  [d (char-set "dD")]
  [e (char-set "eE")]
  [f (char-set "fF")]
  [g (char-set "gG")]
  [h (char-set "hH")]
  [i (char-set "iI")]
  [j (char-set "jJ")]
  [k (char-set "kK")]
  [l (char-set "lL")]
  [m (char-set "mM")]
  [n (char-set "nN")]
  [o (char-set "oO")]
  [p (char-set "pP")]
  [q (char-set "qQ")]
  [r (char-set "rR")]
  [s (char-set "sS")]
  [t (char-set "tT")]
  [u (char-set "uU")]
  [v (char-set "vV")]
  [w (char-set "wW")]
  [x (char-set "xX")]
  [y (char-set "yY")]
  [z (char-set "zZ")]

  [digit (:/ "0" "9")]
  [digit_ (:or digit (:: digit "_"))]
  [digit16 (:/ "af" "AF" "09")]
  [digit16_ (:or digit16 (:: digit16 "_"))]
  [digit8 (:/ "0" "7")]

  [langchar (:or (:/ "az" "AZ" "09") "+" "-" "_")]
  
  ;; What about char->integer constraint?
  [unicode  (:or (:: "u" (:** 1 4 digit16))
                 (:: "U" (:** 1 6 digit16)))]
  
  [character (:or (:: "'" (:~ "'") "'")
                  (:: "'" string-escape "'"))]
  
  [str (:or (:: "\"" (:* string-element (:: "\\" unicode)) "\"")
            byte-str)]
  [byte-str (:: "#\"" (:* byte-string-element) "\"")]
  [string-element (:or (:~ "\"" "\\")
                       string-escape)]
  [byte-string-element (:or (:- (:/ "\x00" "\xFF") "\"" "\\")
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
                      (:: "\\x" (:** 1 2 digit16))
                      (:: "\\" #\newline))]

  [bad-str (:: (:? "#") "\"" 
               (:* (:~ "\"" "\\")
                   (:: "\\" any-char))
               (:? "\\" "\""))]
  [bad-char (:: "'" 
                (:* (:~ "'" "\\")
                    (:: "\\" any-char))
                (:? "\\" "'"))]

  [boolean (:or "#true" "#false")]
                      
  [special-number (:: "#"
                      (:or (:: i n f)
                           (:: n e g i n f)
                           (:: n a n)))]

  [bad-hash (:- (:: "#" (:* non-delims))
                boolean
                special-number)]

  [exponent-marker e]
  [sign (char-set "+-")]
  
  [script (:: "#!" (:or #\space #\/) (:* (:~ #\newline) (:: #\\ #\newline)))]
  
  [identifier (:: (:or (:/ "az" "AZ") "_")
                  (:* (:or (:/ "az" "AZ" "09") "_")))]
  [opchar (:or (:- symbolic (:or))
               (:- punctuation (:or "," ";" ":" "(" ")" "[" "]" "{" "}" "#" "\\")))]
  [operator (:- (:or opchar
                     (:: (:* opchar) (:- opchar "+" "-" ".")))
                "|")]

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

  [non-number-delims (:or non-delims ".")]
  [non-delims (:or alphabetic numeric "_")])

(define (ret name lexeme type paren start-pos end-pos status)
  (values (make-token name lexeme start-pos end-pos)
          type paren (position-offset start-pos) (position-offset end-pos) status))

(define stx-for-original-property (read-syntax #f (open-input-string "original")))
(define current-lexer-source (make-parameter "input"))

(define (make-token name e start-pos end-pos)
  (define offset (position-offset start-pos))
  (token name (datum->syntax #f
                             e
                             (list (current-lexer-source)
                                   (position-line start-pos)
                                   (position-col start-pos)
                                   offset
                                   (- (position-offset end-pos)
                                      offset))
                             stx-for-original-property)))

(define get-next-comment
  (lexer
   ["/*" (values 1 end-pos)]
   ["*/" (values -1 end-pos)]
   [(:or "/" "*" (:* (:~ "*" "/")))
    (get-next-comment input-port)]
   [(eof) (values 'eof end-pos)]
   [(special)
    (get-next-comment input-port)]
   [(special-comment)
    (get-next-comment input-port)]))

(define (read-nested-comment num-opens start-pos input)
  (let-values (((diff end) (get-next-comment input)))
    (cond
      ((eq? 'eof diff) (ret 'fail "" 'error #f start-pos end 'initial))
      (else
       (let ((next-num-opens (+ diff num-opens)))
         (cond
           ((= 0 next-num-opens) (ret 'comment "" 'comment #f start-pos end 'initial))
           (else (read-nested-comment next-num-opens start-pos input))))))))

(define (get-offset i)
  (let-values (((x y offset) (port-next-location i)))
    offset))

(define (escape-regexp s)
  (apply string-append 
         (map (lambda (c)
                (if (memq c '(#\( #\) #\* #\+ #\? #\[ #\] #\. #\^ #\\ #\|))
                    (string #\\ c)
                    (string c)))
              (string->list s))))

(define (special-read-line i)
  (let ((next (peek-char-or-special i)))
    (cond
      ((or (eq? next #\newline) (not (char? next)))
       null)
      (else
       (read-char i)
       (cons next (special-read-line i))))))

(define (read-line/skip-over-specials i)
  (let loop ()
    (let ((next (peek-char-or-special i)))
      (cond
        ((or (eq? next #\newline) (eof-object? next))
         null)
        (else
         (read-char-or-special i)
         (if (char? next)
             (cons next (loop))
             (loop)))))))

(struct s-exp-mode (depth) #:prefab)

(define (lex/status in pos status racket-lexer/status)
  (let-values ([(lexeme type paren start end status)
                (cond
                  [(s-exp-mode? status)
                   (unless racket-lexer/status
                     (error "shouldn't be in S-expression mode without a Racket lexer"))
                   (define depth (s-exp-mode-depth status))
                   (cond
                     [(and (zero? depth)
                           (eqv? #\} (peek-char in)))
                      ;; go out of S-expression mode by using shrubbery lexer again
                      (shrubbery-lexer/status in)]
                     [else
                      (define-values (lexeme type paren start end s-exp-status)
                        (racket-lexer/status in))
                      (values lexeme type paren start end (case s-exp-status
                                                            [(open)
                                                             (s-exp-mode (add1 depth))]
                                                            [(close)
                                                             (s-exp-mode (sub1 depth))]
                                                            [else status]))])]
                  [(eq? status 'continuing)
                   (shrubbery-lexer-continuing/status in)]
                  [else
                   (shrubbery-lexer/status in)])])
    (values lexeme type paren start end 0 status)))

(define-syntax-rule (make-lexer/status number bad-number same-status)
  (lexer
   [(:+ whitespace)
    (ret 'whitespace lexeme 'white-space #f start-pos end-pos 'initial)]
   [(:or str character) (ret 'literal lexeme 'string #f start-pos end-pos 'datum)]
   [bad-number
    (ret 'fail lexeme 'error #f start-pos end-pos 'continuing)]
   [number
    (ret 'literal (parse-number lexeme) 'constant #f start-pos end-pos 'continuing)]
   [special-number
    (let ([num (case lexeme
                 [("#inf") +inf.0]
                 [("#neginf") -inf.0]
                 [("#nan") +nan.0])])
      (ret 'literal num 'constant #f start-pos end-pos 'continuing))]
   [boolean
    (ret 'literal (equal? lexeme "#true") 'constant #f start-pos end-pos 'continuing)]
   ["//"
    (values (make-token 'comment (apply string (read-line/skip-over-specials input-port))
                        start-pos end-pos)
            'comment #f 
            (position-offset start-pos)
            (get-offset input-port)
            'initial)]
   ["/*" (read-nested-comment 1 start-pos input-port)]
   [(:: (:or "#lang " "#!")
        (:or langchar
             (:: langchar (:* (:or langchar "/")) langchar)))
    (ret 'comment lexeme 'other #f start-pos end-pos 'initial)]
   [(:: (:or "#lang " "#!") (:* (:& any-char (complement whitespace))))
    (ret 'fail lexeme 'error #f start-pos end-pos 'initial)]
   [script
    (ret 'comment lexeme 'comment #f start-pos end-pos 'initial)]
   [(:or "(" "[" "{")
    (ret 'opener lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos 'initial)]
   [(:or ")" "]" "}")
    (ret 'closer lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos 'continuing)]
   ["#{"
    (ret 's-exp lexeme 'parenthesis '|{| start-pos end-pos (s-exp-mode 0))]
   [":"
    (ret 'block-operator lexeme 'no-color #f start-pos end-pos 'initial)]
   ["|"
    (ret 'bar-operator lexeme 'no-color #f start-pos end-pos 'initial)]
   ["\\"
    (ret 'continue-operator lexeme 'no-color #f start-pos end-pos 'initial)]
   [","
    (ret 'comma-operator lexeme 'no-color #f start-pos end-pos 'initial)]
   [";"
    (ret 'semicolon-operator lexeme 'no-color #f start-pos end-pos 'initial)]
   [identifier
    (ret 'identifier (string->symbol lexeme) 'symbol #f start-pos end-pos 'continuing)]
   [operator
    (ret 'operator (string->symbol lexeme) 'symbol #f start-pos end-pos 'initial)]
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
   [(eof) (values (make-token 'EOF lexeme start-pos end-pos) 'eof #f #f #f #f)]
   [(:or bad-char bad-str bad-hash)
    (ret 'fail lexeme 'error #f start-pos end-pos 'bad)]
   [any-char (extend-error lexeme start-pos end-pos input-port)]))

(define shrubbery-lexer/status (make-lexer/status number bad-number 'initial))
(define shrubbery-lexer-continuing/status (make-lexer/status number/continuing bad-number/continuing 'continuing))

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
  (string->number (regexp-replace* #rx"_" s "")))

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
    (if (eq? (token-name t) 'bar-operator)
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

(define (syntax->token name s [srcloc #f])
  (if srcloc
      (located-token name s srcloc)
      (token name s)))
