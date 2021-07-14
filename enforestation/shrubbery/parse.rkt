#lang racket/base
(require racket/pretty
         "lex.rkt")

(provide parse-all)

;; Parsing state at the group level:
(struct state (line         ; current group's line and last consumed token's line
               column       ; group's column; below ends group, above starts indented
               paren-immed? ; immediately in `()` or `[]`?
               bar-closes?
               indent-ok?   ; immediately after `:` or `|`?
               delta))      ; column delta created by `\`, applied to `line` continuation

(define (make-state #:line line
                    #:column column
                    #:paren-immed? [paren-immed? #f]
                    #:bar-closes? [bar-closes? #f]
                    #:delta delta)
  (state line
         column
         paren-immed?
         bar-closes?
         #f ; just-saw-colon?
         delta))

;; Parsing state for group sequences: top level, in opener-closer, or after `:`
(struct group-state (closer         ; expected closer: a string, EOF, or column
                     paren-immed?   ; immediately in `()` or `[]`?
                     column         ; expected indentation, just to check
                     check-column?  ; #f => allow any sufficiently large indentation
                     bar-closes?
                     comma-time?    ; allow and expect a comma next
                     last-line      ; most recently consumed line
                     delta))        ; column delta created by `\`, applies to `last-line` continuation

(define (make-group-state #:closer closer
                          #:paren-immed? [paren-immed? #f]
                          #:column column
                          #:check-column? [check-column? #t]
                          #:indent-ok? [indent-ok? #f]
                          #:bar-closes? [bar-closes? #f]
                          #:last-line last-line
                          #:delta delta)
  (group-state closer
               paren-immed?
               column
               check-column?
               bar-closes?
               indent-ok?
               last-line
               delta))

(define closer-column? number?)

(define closer-expected? pair?)
(define (closer-expected closer) (if (pair? closer) (car closer) closer))
(define (closer-expected-opener closer) (and (pair? closer) (cdr closer)))
(define (make-closer-expected str tok) (cons str tok))

;; ----------------------------------------

;; Parse all groups in a stream
(define (parse-top-groups l)
  (define-values (next-l last-line delta) (next-of l #f 0))
  (define-values (gs rest-l end-line end-delta end-t)
    (parse-groups next-l (make-group-state #:closer eof
                                           #:column (if (pair? next-l)
                                                        (token-column (car next-l))
                                                        0)
                                           #:last-line last-line
                                           #:delta delta)))
  (unless (null? rest-l)
    (error "had leftover items" rest-l))
  (datum->syntax #f `(top ,@gs)))

;; Parse a sequence of groups (top level, in opener-closer, or after `:`)
;;   consuming the closer in the case of opener-closer context.
;; Returns: the list of groups
;;          remaining tokens after a closer
;;          line of last consumed (possibly closer)
;;          delta of last consumed
;;          last token consumed (if a closer)
(define (parse-groups l sg)
  (define (done end-t)
    (values null l (group-state-last-line sg) (group-state-delta sg) end-t))
  (define closer (group-state-closer sg))
  (cond
    [(null? l)
     ;; Out of tokens
     (when (string? (closer-expected closer))
       (fail (closer-expected-opener closer) (format "expected ~s" (closer-expected closer))))
     (done #f)]
    [else
     (define t (car l))
     (define column (+ (token-column t) (group-state-delta sg)))
     (cond
       [(and (closer-column? closer)
             (column . < . closer))
        ;; Next token is less indented than this group sequence
        (done #f)]
       [else
        ;; Dispatch on token
        (case (token-name t)
          [(closer)
           (cond
             [(closer-column? closer)
              (done #f)]
             [else
              (unless (equal? (closer-expected closer) (token-e t))
                (fail t (format "expected ~s; closing at ~a" (closer-expected closer) (token-value t))))
              (if (eof-object? (closer-expected closer))
                  ;; continue after extra closer:
                  (parse-groups (cdr l) (struct-copy group-state sg
                                                     [last-line (token-line t)]))
                  ;; stop at closer
                  (values null (cdr l) (token-line t) (group-state-delta sg) t))])]
          [(whitespace comment continue-operator)
           (define-values (next-l last-line delta) (next-of l
                                                            (group-state-last-line sg)
                                                            (group-state-delta sg)))
           (parse-groups (cdr l) (struct-copy group-state sg
                                              [last-line last-line]
                                              [delta delta]))]
          [(comma-operator)
           (cond
             [(closer-column? (group-state-closer sg))
              (done #f)]
             [else
              (unless (and (group-state-paren-immed? sg)
                           (group-state-comma-time? sg))
                (fail t (format "misplaced comma~a"
                                (if (group-state-paren-immed? sg)
                                    ""
                                    " (not immdiately within parentheses or brackets)"))))
              (define-values (rest-l last-line delta) (next-of (cdr l) (token-line t) (group-state-delta sg)))
              ;; In top level or immediately in opener-closer: 
              (parse-groups (cdr l) (struct-copy group-state sg
                                                 [check-column? (next-line? rest-l last-line)]
                                                 [last-line last-line]
                                                 [comma-time? #f]
                                                 [delta delta]))])]
          [(semicolon-operator)
           (when (group-state-paren-immed? sg)
             (fail t (format "misplaced semicolon (~a)"
                             "immdiately within parentheses or brackets")))
           (define-values (rest-l last-line delta) (next-of (cdr l) (token-line t) (group-state-delta sg)))
           (parse-groups rest-l (struct-copy group-state sg
                                             [check-column? (next-line? rest-l last-line)]
                                             [last-line last-line]
                                             [delta delta]))]
          [else
           (when (group-state-comma-time? sg)
             (fail t (format "missing comma before new group (~a)"
                             "within parentheses or braces")))
           (case (token-name t)
             [(bar-operator)
              (cond
                [(group-state-bar-closes? sg)
                 (done #f)]
                [else
                 ;; Bar at the start of a group: no implicit block before,
                 ;; but parse content as a block
                 (define column (+ (token-column t) (group-state-delta sg)))
                 (define line (token-line t))
                 (define same-line? (or (not (group-state-last-line sg))
                                        (= line (group-state-last-line sg))))
                 (when (group-state-check-column? sg)
                   (unless (or same-line?
                               (= column (group-state-column sg)))
                     (fail t "wrong indentation")))
                 (define-values (g rest-l group-end-line group-end-delta)
                   (parse-block l
                                #:closer (column-next column)
                                #:bar-closes? #t
                                #:delta (group-state-delta sg)))
                 (define-values (gs rest-rest-l end-line end-delta end-t)
                   (parse-groups rest-l (struct-copy group-state sg
                                                     [column (if same-line?
                                                                 (group-state-column sg)
                                                                 column)]
                                                     [check-column? #t]
                                                     [last-line group-end-line]
                                                     [delta group-end-delta]
                                                     [comma-time? (group-state-paren-immed? sg)])))
                 (values (cons (list 'group
                                     (add-span-srcloc
                                      t end-t
                                      (cons 'bar g)))
                               gs)
                         rest-rest-l
                         end-line
                         end-delta
                         end-t)])]
             [else
              ;; Parse one group, then recur to continue the sequence:
              (define column (+ (token-column t) (group-state-delta sg)))
              (define line (token-line t))
              (when (group-state-check-column? sg)
                (unless (= column (group-state-column sg))
                  (fail t "wrong indentation")))
              (define-values (g rest-l group-end-line group-delta)
                (parse-group l (make-state #:paren-immed? (group-state-paren-immed? sg)
                                           #:line line
                                           #:column column
                                           #:bar-closes? (group-state-bar-closes? sg)
                                           #:delta (group-state-delta sg))))
              (define-values (gs rest-rest-l end-line end-delta end-t)
                (parse-groups rest-l (struct-copy group-state sg
                                                  [column (if (group-state-check-column? sg)
                                                              column
                                                              (group-state-column sg))]
                                                  [check-column? #t]
                                                  [last-line group-end-line]
                                                  [comma-time? (group-state-paren-immed? sg)]
                                                  [delta group-delta])))
              (values (cons (cons 'group g)
                            gs)
                      rest-rest-l
                      end-line
                      end-delta
                      end-t)])])])]))

;; Parse one group.
;; Returns: the list of items in the group
;;          remaining tokens after group
;;          line of last consumed
;;          delta at last consumed
(define (parse-group l s)
  (define (done)
    (values null l (state-line s) (state-delta s)))
  (cond
    [(null? l) (done)]
    [else
     (define t (car l))
     (define line (token-line t))
     ;; Consume a token
     (define (keep delta)
       (define-values (g rest-l end-line end-delta)
         (parse-group (cdr l) (struct-copy state s
                                           [line line]
                                           [indent-ok? #f]
                                           [delta delta])))
       (values (cons (token-value t) g) rest-l end-line end-delta))
     ;; Dispatch
     (cond
       [(line . > . (state-line s))
        ;; new line
        (case (token-name t)
          [(whitespace comment)
           (parse-group (cdr l) (struct-copy state s
                                             [line line]
                                             [delta 0]))]
          [else
           (define column (token-column t))
           (cond
             [(column . > . (state-column s))
              ;; More indented => forms a group
              ;; Belt and suspenders: require either `:` or `|` to indicate that it's ok
              ;; to start an indented block.
              (unless (or (state-indent-ok? s)
                          (eq? 'bar-operator (token-name t)))
                (fail t "wrong indentation (or missing `:` on previous line)"))
              (define-values (indent-g rest-l end-line end-delta end-t)
                (parse-groups l
                              (make-group-state #:closer column
                                                #:column column
                                                #:last-line (state-line s)
                                                #:delta 0)))
              (values (list (tag-as-block indent-g))
                      rest-l
                      end-line
                      end-delta)]
             [(and (state-paren-immed? s)
                   (eq? 'operator (token-name t)))
              ;; Treat an non-indented leading operator as a continuation of the group
              (keep 0)]
             [else
              (done)])])]
       [else
        ;; Not a new line
        (case (token-name t)
          [(closer comma-operator semicolon-operator)
           (done)]
          [(identifier number literal operator)
           (keep (state-delta s))]
          [(block-operator)
           (parse-block l
                        #:closer (column-half-next (state-column s))
                        #:delta (state-delta s))]
          [(bar-operator)
           (cond
             [(state-bar-closes? s)
              (done)]
             [else
              ;; Implicit `:` before this inline `|`. The nested loop
              ;; here implements that `:`.
              (define bar-column (+ (token-column t) (state-delta s)))
              (let loop ([prev-accum null] [l l])
                (define t (car l))
                (define-values (new-g rest-l end-line end-delta)
                  (case (token-name t)
                    [(bar-operator)
                     (define-values (g rest-l end-line end-delta)
                       (parse-block l
                                    #:closer (column-next (+ (token-column t) (state-delta s)))
                                    #:bar-closes? #t
                                    #:delta (state-delta s)))
                     (values (list 'group
                                   (add-span-srcloc
                                    t #f
                                    (cons 'bar g)))
                             rest-l
                             end-line
                             end-delta)]
                    [else
                     (define-values (g rest-l end-line end-delta)
                       (parse-group l (struct-copy state s
                                                   [line (token-line t)]
                                                   [column bar-column])))
                     (values (cons 'group g)
                             rest-l end-line end-delta)]))
                (define accum (cons new-g prev-accum))
                ;; If next is `|`, absorb it into the implicit block
                (define (done-bar-block)
                  (values (list (tag-as-block (reverse accum)))
                          rest-l
                          end-line
                          end-delta))
                (cond
                  [(null? rest-l) (done-bar-block)]
                  [else
                   (define next-t (car rest-l))
                   (cond
                     ;; Indentation match => part of block
                     [(= bar-column (+ (token-column next-t) (state-delta s)))
                      (loop accum rest-l)]
                     [else
                      (case (token-name next-t)
                        [(bar-operator)
                         (cond
                           ;; Same line => part of block
                           [(= end-line (token-line next-t))
                            (loop accum rest-l)]
                           ;; Less indentation => not part of block
                           [(bar-column . > . (+ (token-column next-t) (state-delta s)))
                            (done-bar-block)]
                           [else
                            (fail next-t "wrong indentation for `|`")
                            (loop accum rest-l)])]
                        [else (done-bar-block)])])]))])]
          [(opener)
           (define-values (closer tag paren-immed?)
             (case (token-e t)
               [("(") (values ")" 'parens #t)]
               [("{") (values "}" 'block #f)]
               [("[") (values "]" 'brackets #t)]
               [else (error "unknown opener" t)]))
           (define-values (next-l last-line delta) (next-of (cdr l) line (state-delta s)))
           (define sub-column
             (if (pair? next-l)
                 (+ (token-column (car next-l)) (state-delta s))
                 (column-next (+ (token-column t) (state-delta s)))))
           (define-values (gs rest-l close-line close-delta end-t)
             (parse-groups next-l (make-group-state #:closer (make-closer-expected closer t)
                                                    #:paren-immed? paren-immed?
                                                    #:column sub-column
                                                    #:last-line last-line
                                                    #:delta delta)))
           
           (define-values (g rest-rest-l end-line end-delta)
             (parse-group rest-l (struct-copy state s
                                              [line close-line]
                                              [delta close-delta]
                                              [indent-ok? #f])))
           (values (cons (add-span-srcloc
                          t end-t
                          (if (eq? tag 'block)
                              (tag-as-block gs)
                              (cons tag gs)))
                         g)
                   rest-rest-l
                   end-line
                   end-delta)]
          [(whitespace comment continue-operator)
           (define-values (next-l line delta) (next-of l (state-line s) (state-delta s)))
           (parse-group next-l (struct-copy state s
                                            [line line]
                                            [delta delta]))]
          [else
           (error "unexpected" t)])])]))

(define (parse-block l
                     #:closer closer
                     #:bar-closes? [bar-closes? #f]
                     #:delta in-delta)
  (define t (car l))
  (define line (token-line t))
  (define-values (next-l last-line delta) (next-of (cdr l) line in-delta))
  (define (block-empty)
    (values (list (tag-as-block null))
            next-l
            line
            delta))
  (cond
    [(pair? next-l)
     (define next-t (car next-l))
     (define-values (indent-gs rest-l end-line end-delta end-t)
       (parse-groups next-l
                     (make-group-state #:closer closer
                                       #:column (+ (token-column next-t) delta)
                                       #:last-line last-line
                                       #:bar-closes? bar-closes?
                                       #:delta delta)))
     (values (or (extract-one-block indent-gs)
                 (list (add-span-srcloc
                        t end-t
                        (tag-as-block indent-gs))))
             rest-l
             end-line
             end-delta)]
    [else
     (block-empty)]))

(define (extract-one-block gs)
  (and (pair? gs)
       (null? (cdr gs))
       (let ([gp (car gs)]) ; (cons 'group something)
         (and (pair? (cdr gp))
              (null? (cddr gp))
              (let ([g (cadr gp)])
                (and (pair? g)
                     (or (tag? 'block (car g))
                         (tag? 'alts (car g)))
                     (list g)))))))

(define (tag-as-block gs)
  (cond
    [(and (pair? gs)
          (for/and ([g (in-list gs)])
            (and (pair? g)
                 (eq? 'group (car g))
                 (pair? (cdr g))
                 (null? (cddr g))
                 (let ([b (cadr g)])
                   (and (pair? b)
                        (tag? 'bar (car b))
                        (pair? (cdr b))
                        (null? (cddr b))
                        (pair? (cadr b))
                        (tag? 'block (caadr b)))))))
     (cons 'alts (for/list ([g (in-list gs)])
                   (let ([b (cadr g)])
                     (cadr b))))]
    [else (cons 'block gs)]))

(define (tag? sym e)
  (or (eq? sym e)
      (and (syntax? e)
           (eq? sym (syntax-e e)))))

(define (add-span-srcloc start-t end-t l)
  (cond
    [(not start-t) l]
    [else
     (define (add-srcloc l loc)
       (cons (datum->syntax #f (car l) loc stx-for-original-property)
             (cdr l)))
     (define last-t/e (or end-t
                          (let loop ([e l])
                            (cond
                              [(syntax? e) e]
                              [(not (pair? e)) #f]
                              [(null? (cdr e))
                               (loop (car e))]
                              [else (loop (cdr e))]))))
     (define s-loc (token-srcloc start-t))
     (define e-loc/e (and last-t/e
                          (if (syntax? last-t/e)
                              last-t/e
                              (token-srcloc last-t/e))))
     (add-srcloc l (vector (srcloc-source s-loc)
                           (srcloc-line s-loc)
                           (srcloc-column s-loc)
                           (srcloc-position s-loc)
                           (let ([s (srcloc-position s-loc)]
                                 [e (if (srcloc? e-loc/e)
                                        (srcloc-position e-loc/e)
                                        (syntax-position e-loc/e))]
                                 [sp (if (srcloc? e-loc/e)
                                         (srcloc-span e-loc/e)
                                         (syntax-span e-loc/e))])
                             (and s e sp
                                  (+ (- e s) sp)))))]))
   
;; Consume whitespace and comments, including continuing backslashes,
;; where lookahead is needed
;; Arguments/returns:
;;   list of input tokens (after whitespace, comments, and continues)
;;   most recently consumed non-whitesace line (to determine whether
;;     next is on same line); on input; the line can be #f, which means
;;     "treat as same line"; the result is never #f
;;   current line delta (created by continues)
(define (next-of l last-line delta)
  (cond
    [(null? l) (values null (or last-line 0) delta)]
    [else
     (define t (car l))
     (case (token-name t)
       [(whitespace comment)
        (next-of (cdr l) last-line delta)]
       [(continue-operator)
        (define line (token-line t))
        ;; a continue operator not followed only by whitespace and
        ;; comments is just treated as whitespace
        (define next-l (let loop ([l (cdr l)])
                         (cond
                           [(null? l) null]
                           [else (case (token-name (car l))
                                   [(whitespace comment) (loop (cdr l))]
                                   [else l])])))
        (cond
          [(and (pair? next-l)
                (= line (token-line (car next-l))))
           ;; like whitespace:
           (next-of next-l last-line delta)]
          [else
           (next-of next-l
                    ;; whitespace-only lines don't count, so next continues
                    ;; on the same line by definition:
                    #f
                    (+ (if (or (not last-line) (= line last-line))
                           delta
                           0)
                       (token-column t) 1))])]
       [else
        (define line (token-line t))
        (values l
                (or last-line line)
                (if (or (not last-line) (= line last-line))
                    delta
                    0))])]))

(define (next-line? l last-line)
  ((token-line (car l)) . > . last-line))

;; Report an error on failure, but then keep parsing anyway
;;  if in recover mode
(define current-recover-mode (make-parameter #f))
(define (fail tok msg)
  (define loc (if (syntax? tok)
                  (srcloc (syntax-source tok)
                          (syntax-line tok)
                          (syntax-column tok)
                          (syntax-position tok)
                          (syntax-span tok))
                  (token-srcloc tok)))
  (cond
    [(current-recover-mode)
     (log-error "~a: ~a" (srcloc->string loc) msg)]
    [else
     (raise
      (exn:fail:read
       (if (error-print-source-location)
           (format "~a: ~a" (srcloc->string loc) msg)
           msg)
       (current-continuation-marks)
       (list loc)))]))

(define (column-next c)
  (if (integer? c)
      (add1 c)
      (add1 (inexact->exact (floor c)))))

(define (column-half-next c)
  (if (integer? c)
      (+ c 0.5)
      (column-next c)))

;; ----------------------------------------

(define (parse-all in)
  (define l (lex-all in fail))
  (if (null? l)
      eof
      (parse-top-groups l)))
  
(module+ main
  (require racket/cmdline)

  (define (parse-all* in)
    (port-count-lines! in)
    (define e (parse-all in))
    (unless (eof-object? e)
      (pretty-write
       (syntax->datum e))))

  (command-line
   #:once-each
   [("--recover") "Continue parsing after an error"
                  (current-recover-mode #t)]
   #:args file
   (if (null? file)
       (parse-all* (current-input-port))
       (for-each (lambda (file)
                   (call-with-input-file*
                    file
                    parse-all*))
                 file))))
