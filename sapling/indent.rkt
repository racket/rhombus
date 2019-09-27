#lang racket/base
(require racket/pretty
         "private/lex.rkt")

(define INDENT 2)

;; Parsing and indenting state at all levels:
(struct any-state (line          ; current line
                   indent        ; current indent
                   or-indent     ; #f or number; number => indent for next `|`
                   and-indent))  ; #f or number; number => indent for next `&`

;; Parsing state at the group level:
(struct state any-state (after-conj    ; #f or 'or or 'and: after `|` or `&` in group?
                         line-indent)) ; indentation for `\`

;; Parsing state at the group-sequence level for top and parens:
(struct group-state any-state (closer  ; expected closer, a string or EOF
                               closer-indent))

;; Parsing state at the group-sequence level for ":", "|", and "&":
(struct nested-group-state any-state (after-conj
                                      line-indent))

;; Emit all groups in a stream
(define (indent-top-groups l)
  (indent-groups l (group-state -1 0 #f #f eof 0))
  (void))

;; Emit a sequence of groups, top level or in opener-closer,
;;   including the closer.
;; Returns: remaining tokens after a closer
;;          closer's line
(define (indent-groups l sg)
  (cond
    [(null? l)
     (values null #f)]
    [else
     (define t (car l))
     (define line (token-line t))
     (case (token-name t)
       [(closer)
        (emit t (struct-copy any-state sg
                             [indent (group-state-closer-indent sg)]))
        (define closer (group-state-closer sg))
        (unless (equal? closer (token-e t))
          'wrong-closer)
        (if (eof-object? closer)
            ;; continue after extra closer:
            (indent-groups (cdr l) (struct-copy group-state sg
                                                [line #:parent any-state line]))
            ;; stop at closer
            (values (cdr l) line))]
       [(comma-operator semicolon-operator comment)
        (emit t sg)
        (indent-groups (cdr l) (struct-copy group-state sg
                                            [line #:parent any-state line]))]
       [(whitespace)
        (emit-whitespace t sg)
        (indent-groups (cdr l) sg)]
       [else
        (emit-indent t sg)
        (define-values (rest-l group-end-s) 
          (indent-group l (state line
                                 (any-state-indent sg)
                                 (any-state-or-indent sg)
                                 (any-state-and-indent sg)
                                 #f
                                 (+ (any-state-indent sg) INDENT))))
        (indent-groups rest-l  (struct-copy group-state sg
                                            [line #:parent any-state (any-state-line group-end-s)]
                                            #;[or-indent #:parent any-state (any-state-or-indent group-end-s)]
                                            #;[and-indent #:parent any-state (any-state-and-indent group-end-s)]))])]))

(define (emit-indent t s)
  (unless (equal? (token-line t) (any-state-line s))
    (write-string (make-string (any-state-indent s) #\space))))

(define (emit t s)
  (emit-indent t s)
  (case (token-name t)
    [(number)
     (write-string (number->string (token-e t)))]
    [(literal)
     (define v (token-e t))
     (if (boolean? v)
         (write-string (if v "#true" "#false"))
         (write-string v))]
    [(opener closer comment whitespace)
     (write-string (token-e t))]
    [else
     (write-string (symbol->string (token-e t)))]))

(define (emit-whitespace t s)
  (define str (token-e t))
  (define line (token-line t))
  (cond
    [(and (equal? line (any-state-line s))
          (not (regexp-match? #rx"\n" str)))
     (write-string str)]
    [else
     (define newlines (regexp-replace* #rx"[^\n]" str ""))
     (write-string newlines)]))

(define (current-column)
  (define-values (out-line out-col out-pos) (port-next-location (current-output-port)))
  out-col)

;; Indent a sequence of groups under ":", "|", or "&"
;; Returns: remaining tokens after sequence
;;          ending state
(define (indent-nested-groups l nsg)
  (define (done)
    (values l nsg))
  (cond
    [(null? l)
     (done)]
    [else
     (define t (car l))
     (define line (token-line t))
     (define (keep-group)
       (define-values (rest-l group-end-s)
         (indent-group l (state line
                                (any-state-indent nsg)
                                (any-state-or-indent nsg)
                                (any-state-and-indent nsg)
                                (nested-group-state-after-conj nsg)
                                (nested-group-state-line-indent nsg))))
       (indent-nested-groups rest-l (struct-copy nested-group-state nsg
                                                 [line #:parent any-state (any-state-line group-end-s)]
                                                 [or-indent #:parent any-state (any-state-or-indent group-end-s)]
                                                 [and-indent #:parent any-state (any-state-and-indent group-end-s)])))
     (cond
       [(line . > . (add1 (any-state-line nsg)))
        (done)]
       [else
        (case (token-name t)
          [(closer comma-operator semicolon-operator)
           (done)]
          [(comment)
           (emit t nsg)
           (indent-nested-groups (cdr l) (struct-copy nested-group-state nsg
                                                      [line #:parent any-state line]))]
          [(whitespace)
           (emit-whitespace t nsg)
           (indent-nested-groups (cdr l) nsg)]
          [(or-operator)
           (if (nested-group-state-after-conj nsg)
               (done)
               (keep-group))]
          [(and-operator)
           (if (eq? 'and (nested-group-state-after-conj nsg))
               (done)
               (keep-group))]
          [(block-operator)
           (if (eq? 'and (nested-group-state-after-conj nsg))
               (done)
               (begin
                 (emit-indent t nsg)
                 (keep-group)))]
          [else
           (emit-indent t nsg)
           (keep-group)])])]))

;; Indent one group.
;; Returns: remaining tokens after group
;;          ending state
(define (indent-group l s)
  (define (done)
    (values l s))
  (cond
    [(null? l) (done)]
    [else
     (define t (car l))
     (define line (token-line t))
     (define (keep)
       (emit t s)
       (indent-group (cdr l) (struct-copy state s [line #:parent any-state line])))
     (define new-group? (line . > . (add1 (any-state-line s))))
     (cond
       [new-group?
        (done)]
       [else
        (define new-line? (line . > . (any-state-line s)))
        (define (keep-nested-group #:after-conj [after-conj (state-after-conj s)]
                                   #:indent-to [indent-to #f]
                                   #:get-or-indent [get-or-indent any-state-or-indent]
                                   #:get-and-indent [get-and-indent any-state-and-indent]
                                   #:indent-mode [indent-mode 'sub])
          (emit t (if indent-to
                      (struct-copy any-state s
                                   [indent indent-to])
                      s))
          (define next-line (next-line-of (cdr l)))
          (define indent (cond
                           [(and (eq? indent-mode 'block)
                                 next-line
                                 (next-line . > . line))
                            (+ (any-state-indent s) INDENT)]
                           [(eq? indent-mode 'none)
                            (any-state-indent s)]
                           [else
                            (+ (current-column) 1)]))
          (define or-indent (get-or-indent s))
          (define and-indent (get-and-indent s))
          (define nsg
            (nested-group-state (or next-line
                                    line)
                                indent
                                or-indent
                                and-indent
                                after-conj
                                (+ indent INDENT)))
          (define nested-l
            (if (and next-line
                     (not (= next-line line)))
                (emit-up-to-next (cdr l) (any-state line
                                                       indent
                                                       or-indent
                                                       and-indent))
                (cdr l)))
          (define-values (rest-l nested-end-nsg)
            (indent-nested-groups nested-l nsg))
          (indent-group rest-l (struct-copy state s
                                            [line #:parent any-state (any-state-line nested-end-nsg)]
                                            [or-indent #:parent any-state (any-state-or-indent nested-end-nsg)]
                                            [and-indent #:parent any-state (any-state-and-indent nested-end-nsg)])))
        (case (token-name (car l))
          [(identifier number literal)
           (if new-line?
               (done)
               (keep))]
          [(operator)
           (cond
             [new-line?
              ;; Doesn't matter for indentation whether we're in parens
              (done)]
             [else
              (keep)])]
          [(continue-operator)
           (cond
             [new-line?
              (indent-group (cdr l) s)]
             [else
              (emit t s)
              (define next-line (next-line-of (cdr l)))
              (cond
                [(and next-line
                      (= next-line (add1 line)))
                 (define next-l
                   (emit-up-to-next (cdr l) (any-state line
                                                       (state-line-indent s)
                                                       #f
                                                       #f)))
                 (indent-group next-l (struct-copy state s
                                                   [line #:parent any-state next-line]))]
                [else
                 (indent-group (cdr l) s)])])]
          [(opener)
           (cond
             [new-line?
              (done)]
             [else
              (emit t s)
              (define out-col (current-column))
              (define closer
                (case (token-e t)
                  [("(") ")"]
                  [("[") "]"]
                  [("{") "}"]
                  [else (error "unknown opener" t)]))
              (define next-line (next-line-of (cdr l)))
              (define col-indent? (and next-line (= next-line line)))
              (define indent
                (if col-indent?
                    out-col
                    (+ (any-state-indent s) INDENT)))
              (define-values (rest-l close-line)
                (indent-groups (cdr l)
                               (group-state line
                                            indent
                                            #f
                                            #f
                                            closer
                                            (if col-indent?
                                                indent
                                                (any-state-indent s)))))
              (indent-group rest-l (struct-copy state s
                                                [line #:parent any-state close-line]))])]
          [(closer comma-operator semicolon-operator)
           (done)]
          [(block-operator)
           (if (or new-line?
                   (eq? 'and (state-after-conj s)))
               (done)
               (keep-nested-group #:indent-mode (if (next-is-curly? (cdr l))
                                                    'none
                                                    'block)))]
          [(or-operator)
           (if (state-after-conj s)
               (done)
               (keep-nested-group #:after-conj 'or
                                  #:indent-to (any-state-or-indent s)
                                  #:get-or-indent (lambda (s) (or (any-state-or-indent s)
                                                                  (sub1 (current-column))))
                                  #:get-and-indent (lambda (s) #f)))]
          [(and-operator)
           (if (eq? 'and (state-after-conj s))
               (done)
               (keep-nested-group #:after-conj 'and
                                  #:indent-to (any-state-and-indent s)
                                  #:get-and-indent (lambda (s) (or (any-state-and-indent s)
                                                                   (sub1 (current-column))))))]
          [(comment)
           (emit t s)
           (indent-group (cdr l) (struct-copy state s
                                              [line #:parent any-state line]))]
          [(whitespace)
           (emit-whitespace t s)
           (indent-group (cdr l) s)]
          [else
           (error "unexpected" t)])])]))

;; Lookahead
(define (next-line-of l)
  (cond
    [(null? l) #f]
    [else
     (case (token-name (car l))
       [(whitespace comment) (next-line-of (cdr l))]
       [else
        (token-line (car l))])]))

(define (next-is-curly? l)
  (cond
    [(null? l) #f]
    [else
     (define t (car l))
     (case (token-name t)
       [(whitespace comment) (next-is-curly? (cdr l))]
       [(opener)
        (equal? (token-e t) "{")]
       [else #f])]))

(define (emit-up-to-next l s)
  (define t (car l))
  (define line (token-line t))
  (case (token-name t)
    [(comment)
     (emit t s)
     (emit-up-to-next (cdr l) (struct-copy any-state s
                                           [line line]))]
    [(whitespace)
     (emit-whitespace t s)
     (emit-up-to-next (cdr l) s)]
    [(or-operator)
     (define or-indent (any-state-or-indent s))
     (emit-indent t (if or-indent
                        (struct-copy any-state s
                                     [indent or-indent])
                        s))
     l]
    [(and-operator)
     (define and-indent (any-state-and-indent s))
     (emit-indent t (if and-indent
                        (struct-copy any-state s
                                     [indent and-indent])
                        s))
     l]
    [else
     (emit-indent t s)
     l]))

(define (indent-all in)
  (define l (lex-all in))
  (unless (null? l)
    (define o (open-output-bytes))
    (port-count-lines! o)
    (parameterize ([current-output-port o])
      (indent-top-groups l))
    (write-bytes (get-output-bytes o))
    (void)))
  
(module+ main
  (require racket/cmdline)

  (command-line
   #:args file
   (if (null? file)
       (indent-all (current-input-port))
       (for-each (lambda (file)
                   (call-with-input-file*
                    file
                    indent-all))
                 file))))
