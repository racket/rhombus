#lang racket/base
(require racket/pretty
         "private/lex.rkt")

;; Parsing state at the group level:
(struct state (line         ; current group's line
               column       ; group's column; below ends group, above starts indented
               paren-immed? ; immediately in `()` or `[]`?
               bar-closes?
               last-line))  ; most recently consumed line

(define (make-state #:line line
                    #:column column
                    #:paren-immed? [paren-immed? #f]
                    #:bar-closes? [bar-closes? #f]
                    #:last-line last-line)
  (state line
         column
         paren-immed?
         bar-closes?
         last-line))

;; Parsing state for group sequences: top level, in opener-closer, or after `:`
(struct group-state (closer         ; expected closer: a string, EOF, or column
                     paren-immed?   ; immediately in `()` or `[]`?
                     column         ; expected indentation, just to check
                     check-column?  ; #f => allow any sufficiently large indentation
                     bar-closes?
                     last-line))    ; most recently consumed line

(define (make-group-state #:closer closer
                          #:paren-immed? [paren-immed? #f]
                          #:column column
                          #:check-column? [check-column? #t]
                          #:bar-closes? [bar-closes? #f]
                          #:last-line last-line)
  (group-state closer
               paren-immed?
               column
               check-column?
               bar-closes?
               last-line))

(define closer-column? exact-integer?)

;; ----------------------------------------

;; Parse all groups in a stream
(define (parse-top-groups l)
  (define-values (next-l next-line) (next-of l #f))
  (define-values (gs rest-l end-line)
    (parse-groups next-l (make-group-state #:closer eof
                                           #:column (if (pair? next-l)
                                                        (token-column (car next-l))
                                                        0)
                                           #:last-line next-line)))
  (unless (null? rest-l)
    (error "had leftover items" rest-l))
  (datum->syntax #f `(top ,@gs)))

;; Parse a sequence of groups (top level, in opener-closer, or after `:`)
;;   consuming the closer in the case of opener-closer context.
;; Returns: the list of groups
;;          remaining tokens after a closer
;;          line of last consumed (possibly closer)
(define (parse-groups l sg)
  (define (done)
    (values null l (group-state-last-line sg)))
  (define closer (group-state-closer sg))
  (cond
    [(null? l)
     ;; Out of tokens
     (when (string? closer)
       (fail (format "expected ~s" closer)))
     (done)]
    [else
     (define t (car l))
     (define column (token-column t))
     (cond
       [(and (closer-column? closer)
             (column . < . closer))
        ;; Next token is less indented than this group sequence
        (done)]
       [else
        ;; Dispatch on token
        (case (token-name t)
          [(closer)
           (cond
             [(closer-column? closer)
              (done)]
             [else
              (unless (equal? closer (token-e t))
                (fail (format "expected ~s; closing at ~a" closer (token-value t))))
              (if (eof-object? closer)
                  ;; continue after extra closer:
                  (parse-groups (cdr l) (struct-copy group-state sg
                                                     [last-line (token-line t)]))
                  ;; stop at closer
                  (values null (cdr l) (token-line t)))])]
          [(comment)
           (parse-groups (cdr l) (struct-copy group-state sg
                                              [last-line (token-line t)]))]
          [(whitespace) (parse-groups (cdr l) sg)]
          [(comma-operator)
           (cond
             [(closer-column? (group-state-closer sg))
              (done)]
             [else
              (unless (group-state-paren-immed? sg)
                (fail (format (format "misplaced comma at line ~a column ~a"
                                      (token-line t)
                                      (token-column t)))))
              
              ;; In top level or immediately in opener-closer: 
              (define-values (gs rest-l end-line)
                (parse-groups (cdr l) (struct-copy group-state sg
                                                   [check-column? #f]
                                                   [last-line (token-line t)])))
              (values (cons (token-value t) gs)
                      rest-l
                      end-line)])]
          [(semicolon-operator)
           (parse-groups (cdr l) (struct-copy group-state sg
                                              [check-column? #f]
                                              [last-line (token-line t)]))]
          [(bar-operator)
           (cond
             [(group-state-bar-closes? sg)
              (done)]
             [else
              ;; Bar at the start of a group: no implicit block before,
              ;; but parse content as a block
              (define column (token-column t))
              (define line (token-line t))
              (define same-line? (or (not (group-state-last-line sg))
                                     (= line (group-state-last-line sg))))
              (when (group-state-check-column? sg)
                (unless (or same-line?
                            (= column (group-state-column sg)))
                  (fail (format "wrong indentation at line ~a column ~a" line column))))
              (define-values (g rest-l group-end-line) 
                (parse-block l
                             #:closer (add1 column)
                             #:bar-closes? #t))
              (define-values (gs rest-rest-l end-line)
                (parse-groups rest-l (struct-copy group-state sg
                                                  [column (if same-line?
                                                               (group-state-column sg)
                                                              column)]
                                                  [check-column? #t]
                                                  [last-line group-end-line])))
              (values (cons (list 'group (cons 'bar g)) gs)
                      rest-rest-l
                      end-line)])]
          [else
           ;; Parse one group, then recur to continue the sequence:
           (define column (token-column t))
           (define line (token-line t))
           (when (group-state-check-column? sg)
             (unless (= column (group-state-column sg))
               (fail (format "wrong indentation at line ~a column ~a" line column))))
           (define-values (g rest-l group-end-line) 
             (parse-group l (make-state #:paren-immed? (group-state-paren-immed? sg)
                                        #:line line
                                        #:column column
                                        #:bar-closes? (group-state-bar-closes? sg)
                                        #:last-line (group-state-last-line sg))))
           (define-values (gs rest-rest-l end-line)
             (parse-groups rest-l (struct-copy group-state sg
                                               [column (if (group-state-check-column? sg)
                                                           column
                                                           (group-state-column sg))]
                                               [check-column? #t]
                                               [last-line group-end-line])))
           (values (cons (cons 'group g) gs)
                   rest-rest-l
                   end-line)])])]))

;; Parse one group.
;; Returns: the list of items in the group
;;          remaining tokens after group
;;          line of last consumed
(define (parse-group l s)
  (define (done)
    (values null l (state-last-line s)))
  (cond
    [(null? l) (done)]
    [else
     (define t (car l))
     (define line (token-line t))
     ;; Consume a token
     (define (keep)
       (define-values (g rest-l end-line)
         (parse-group (cdr l) (struct-copy state s
                                           [line line]
                                           [last-line line])))
       (values (cons (token-value t) g) rest-l end-line))
     ;; Dispatch
     (cond
       [(line . > . (state-line s))
        ;; new line
        (case (token-name t)
          [(whitespace)
           (parse-group (cdr l) s)]
          [(comment)
           (parse-group (cdr l) (struct-copy state s
                                             [line line]
                                             [last-line line]))]
          [else
           (define column (token-column t))
           (cond
             [(column . > . (state-column s))
              (define-values (indent-g rest-l end-line)
                (parse-groups l
                              (make-group-state #:closer column
                                                #:column column
                                                #:last-line (state-last-line s))))
              (values (list (cons 'block indent-g))
                      rest-l
                      end-line)]
             [(and (state-paren-immed? s)
                   (eq? 'operator (token-name t)))
              ;; Treat an non-indented leading operator as a continuation of the group
              (keep)]
             [else
              (done)])])]
       [else
        ;; Not a new line
        (case (token-name t)
          [(closer comma-operator semicolon-operator)
           (done)]
          [(identifier number literal operator)
           (keep)]
          [(continue-operator)
           (define-values (next-l prev-line) (next-of (cdr l) line))
           (define (continue-done)
             (values null next-l line))
           (cond
             [(pair? next-l)
              (define next-t (car next-l))
              (case (token-name next-t)
                [(colon-operator semicolon-operator)
                 (continue-done)]
                [else
                 (define next-line (token-line next-t))
                 (cond
                   [(<= next-line (add1 prev-line))
                    (parse-group next-l (struct-copy state s
                                                     [line next-line]
                                                     [column 0]
                                                     [last-line prev-line]))])])]
             [else (continue-done)])]
          [(block-operator)
           (parse-block l #:closer (add1 (state-column s)))]
          [(bar-operator)
           (cond
             [(state-bar-closes? s)
              (done)]
             [else
              ;; Implicit `:` before this inline `|`. The nested loop
              ;; here implements that `:`.
              (define bar-column (token-column t))
              (let loop ([prev-accum null] [l l])
                (define t (car l))
                (define-values (new-g rest-l end-line)
                  (case (token-name t)
                    [(bar-operator)
                     (define-values (g rest-l end-line)
                       (parse-block l
                                    #:closer (add1 (token-column t))
                                    #:bar-closes? #t))
                     (values (list 'group (cons 'bar g))
                             rest-l
                             end-line)]
                    [else
                     (define-values (g rest-l end-line)
                       (parse-group l (struct-copy state s
                                                   [line (token-line t)]
                                                   [column bar-column])))
                     (values (cons 'group g) rest-l end-line)]))
                (define accum (cons new-g prev-accum))
                ;; If next is `|`, absorb it into the implicit block
                (define (done-bar-block)
                  (values (list (cons 'block (reverse accum)))
                          rest-l
                          end-line))
                (cond
                  [(null? rest-l) (done-bar-block)]
                  [else
                   (define next-t (car rest-l))
                   (cond
                     ;; Indentation match => part of block
                     [(= bar-column (token-column next-t))
                      (loop accum rest-l)]
                     [else
                      (case (token-name next-t)
                        [(bar-operator)
                         (cond
                           ;; Same line => part of block
                           [(= end-line (token-line next-t))
                            (loop accum rest-l)]
                           ;; Less indentation => not part of block
                           [(bar-column . > . (token-column next-t))
                            (done-bar-block)]
                           [else
                            (fail (format "wrong indentation for `|` at line ~a column ~a"
                                          (token-line next-t)
                                          (token-column next-t)))
                            (loop accum rest-l)])]
                        [else (done-bar-block)])])]))])]
          [(opener)
           (define-values (closer tag paren-immed?)
             (case (token-e t)
               [("(") (values ")" 'parens #t)]
               [("{") (values "}" 'block #f)]
               [("[") (values "]" 'brackets #t)]
               [else (error "unknown opener" t)]))
           (define-values (next-l prev-line) (next-of (cdr l) line))
           (define sub-column
             (if (pair? next-l)
                 (token-column (car next-l))
                 (add1 (token-column t))))
           (define-values (gs rest-l close-line)
             (parse-groups next-l (make-group-state #:closer closer
                                                    #:paren-immed? paren-immed?
                                                    #:column sub-column
                                                    #:last-line prev-line)))
           (define-values (g rest-rest-l end-line)
             (parse-group rest-l (struct-copy state s
                                              [line close-line]
                                              [last-line close-line])))
           (values (cons (cons tag gs)
                         g)
                   rest-rest-l
                   end-line)]
          [(whitespace)
           (parse-group (cdr l) s)]
          [(comment)
           (parse-group (cdr l) (struct-copy state s
                                             [last-line line]))]
          [else
           (error "unexpected" t)])])]))

(define (parse-block l
                     #:closer closer
                     #:bar-closes? [bar-closes? #f])
  (define t (car l))
  (define line (token-line t))
  (define-values (next-l prev-line) (next-of (cdr l) line))
  (define (block-empty)
    (values (list (list 'block))
            next-l
            line))
  (cond
    [(pair? next-l)
     (define next-t (car next-l))
     (define next-line (token-line next-t))
     (define (block-at)
       (define-values (indent-gs rest-l end-line)
         (parse-groups next-l
                       (make-group-state #:closer closer
                                         #:column (token-column next-t)
                                         #:last-line prev-line
                                         #:bar-closes? bar-closes?)))
       (values (or (extract-one-block indent-gs)
                   (list (cons 'block indent-gs)))
               rest-l
               end-line))
     (cond
       [(= next-line line)
        (block-at)]
       [(= next-line (add1 prev-line))
        (block-at)]
       [else
        (block-empty)])]
    [else
     (block-empty)]))

(define (extract-one-block gs)
  (and (pair? gs)
       (null? (cdr gs))
       (let ([gp (car gs)]) ; (cons '%#grp something)
         (and (pair? (cdr gp))
              (null? (cddr gp))
              (let ([g (cadr gp)])
                (and (pair? g)
                     (eq? (car g) 'block)
                     (list g)))))))
   
;; Consume whitespace and comments where lookahead is needed
;; Returns:
;;   list of tokens starting with non-whitespace/comment
;;   last consumed line (incremented for comments)
(define (next-of l line)
  (cond
    [(null? l) (values l line)]
    [else
     (define t (car l))
     (case (token-name t)
       [(whitespace) (next-of (cdr l) line)]
       [(comment)
        (define c-line (token-line t))
        (next-of (cdr l)
                 (and line
                      (if (= c-line (add1 line))
                          c-line
                          line)))]
       [else (values l line)])]))

;; Report an error on failure, but then keep parsing anyway
;;  if in recover mode
(define current-recover-mode (make-parameter #f))
(define (fail msg)
  (cond
    [(current-recover-mode)
     (log-error msg)]
    [else
     (raise-user-error 'parse "~a" msg)]))

;; ----------------------------------------

(define (parse-all in)
  (define l (lex-all in))
  (unless (null? l)
    (pretty-write
     (syntax->datum (parse-top-groups l)))))
  
(module+ main
  (require racket/cmdline)

  (command-line
   #:once-each
   [("--recover") "Continue parsing after an error"
                  (current-recover-mode #t)]
   #:args file
   (if (null? file)
       (parse-all (current-input-port))
       (for-each (lambda (file)
                   (call-with-input-file*
                    file
                    parse-all))
                 file))))
