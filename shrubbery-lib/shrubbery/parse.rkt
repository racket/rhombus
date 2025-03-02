#lang racket/base
(require racket/pretty
         racket/syntax-srcloc
         "lex.rkt"
         (rename-in "private/column.rkt"
                    [column+ lex:column+])
         "print.rkt"
         "private/property.rkt"
         "private/at-space.rkt")

(provide parse-all)

;; Parsing state at the group level:
(struct state (count?          ; parsed based on lines and columns?
               line            ; current group's line and last consumed token's line
               column          ; group's column; below ends group, above starts indented
               bar-column      ; not #f => use instead of `column` for `|` start
               operator-column ; column for operator that continues group on a new line
               paren-immed     ; immediately in `()` or `[]`: #f, 'normal, or 'at
               bar-closes?     ; does `|` always end a group?
               bar-closes-line ; `|` (also) ends a group on this line
               block-mode      ; 'inside, #f, in-block-mode with `:` or `|` token, or 'end
               can-empty?      ; in a context where a `:` can be empty?
               delta           ; a `cont-delta`, tracks `\` continuations
               raw             ; reversed whitespace (and comments) to be remembered
               at-mode))       ; `@` continuation after term: #f or `at-mode`

(define (make-state #:count? count?
                    #:line line
                    #:column column
                    #:bar-column [bar-column #f]
                    #:operator-column [operator-column #f]
                    #:paren-immed [paren-immed #f]
                    #:bar-closes? [bar-closes? #f]
                    #:bar-closes-line [bar-closes-line #f]
                    #:block-mode [block-mode #f]
                    #:at-mode [at-mode #f]
                    #:can-empty? [can-empty? #t]
                    #:delta delta
                    #:raw [raw null])
  (state count?
         line
         column
         bar-column
         operator-column
         paren-immed
         bar-closes?
         bar-closes-line
         block-mode
         can-empty?
         delta
         raw
         at-mode))

(struct at-mode (rev-prefix
                 initial?
                 splice?
                 stop-at-at?
                 stop-at-next-at?))

(define (make-at-mode #:rev-prefix [rev-prefix '()]
                      #:initial? [initial? #f]
                      #:splice? [splice? #f]
                      #:stop-at-at? [stop-at-at? #f]
                      #:stop-at-next-at? [stop-at-next-at? #f])
  (at-mode rev-prefix
           initial?
           splice?
           stop-at-at?
           stop-at-next-at?))

;; Parsing state for group sequences: top level, in opener-closer, or after `:`
(struct group-state (count?         ; parsed based on lines and columns?
                     closer         ; expected closer: a string, EOF, or column (maybe 'any as column)
                     paren-immed    ; immediately in `()` or `[]`: #f, 'normal, or 'at
                     column         ; not #f => required indentation check checking
                     bar-column     ; not #f => use instead of `column` for blocks
                     check-column?  ; #f => allow any sufficiently large (based on closer) indentation
                     bar-closes?    ; does `|` always end the sequence of groups?
                     bar-closes-line ; `|` (also) ends a sequence of groups on this line
                     block-mode     ; 'inside, #f, in-block-mode with `:` or `|` token, or 'end
                     can-empty?      ; in a context where a `:` can be empty?
                     comma-time?    ; allow and expect a comma next
                     sequence-mode  ; 'any, 'one, or 'none
                     last-line      ; most recently consumed line
                     delta          ; a `cont-delta`, tracks `\` continuations
                     commenting     ; pending group-level `#//` token; exclusive with `tail-commenting`
                     tail-commenting ; pending group-level `#//` at end (so far)
                     raw))          ; reversed whitespace (and comments) to be remembered

(struct in-block-mode (token parent-column))

(define (make-group-state #:count? count?
                          #:closer closer
                          #:paren-immed [paren-immed #f]
                          #:column column
                          #:check-column? [check-column? count?]
                          #:bar-closes? [bar-closes? #f]
                          #:bar-closes-line [bar-closes-line #f]
                          #:block-mode [block-mode #f]
                          #:can-empty? [can-empty? #t]
                          #:sequence-mode [sequence-mode 'any]
                          #:last-line last-line
                          #:delta delta
                          #:commenting [commenting #f]
                          #:raw [raw null])
  (group-state count?
               closer
               paren-immed
               column
               #f
               check-column?
               bar-closes?
               bar-closes-line
               block-mode
               can-empty?
               #f
               sequence-mode
               last-line
               delta
               #f
               commenting
               raw))

(struct cont-delta (column      ; accumulated column offset created by `\` continuations
                    line-span)) ; number of extra lines accumulated by `\` continuations

(define zero-delta (cont-delta 0 0))

(define (closer-column? c) (or (number? c) (and (pair? c) (number? (car c))) (eq? c 'any)))

(define (closer-expected? closer) (and (pair? closer) (not (number? (car closer)))))
(define (closer-expected closer) (if (pair? closer) (car closer) closer))
(define (closer-expected-opener closer) (and (pair? closer) (cdr closer)))
(define (make-closer-expected str tok) (cons str tok))

(define (syntax-raw-identifier-property stx)
  (syntax-property (syntax-raw-property stx '()) 'identifier-as-keyword #t))

(define group-tag (syntax-raw-identifier-property (datum->syntax #f 'group)))
(define top-tag (syntax-raw-identifier-property (datum->syntax #f 'multi)))
(define parens-tag (syntax-raw-identifier-property (datum->syntax #f 'parens)))
(define brackets-tag (syntax-raw-identifier-property (datum->syntax #f 'brackets)))
(define alts-tag (syntax-raw-identifier-property (datum->syntax #f 'alts)))

(define within-parens-str "within parentheses, brackets, or braces")

(define (make-incomparable t)
  (lambda ()
    (fail t "incomparable indentation due to mixed tabs")))

;; ----------------------------------------

;; In the parsed representation of a shrubbery, source locations are
;; not associated with sequences tagged `group` or `top` (but they may
;; have 'raw-prefix and/or 'raw-tail, as described below).
;;
;; * For terms tagged with `parens`, `braces`, `brackets`, `quotes`,
;;   and `block`, the same source location is associated with the tag and
;;   the parentheses, etc., that group the tag with its members, and
;;   that location spans the content and any delimiters used to form
;;   it (such as parentheses or a `:` that starts a block).
;;
;; * For terms tagged with `op`, the source location of the operator
;;   is copied to the `op` tag and the parentheses.
;;
;; * For terms tagged with `alts` or groups tagged with `group`, the
;;   tag has no source location, but the enclosing parentheses get
;;   a source location spanning the content.
;;
;; Raw text is preserved as syntax properties so that the original
;; program text can be reconstructed. Raw text is represented as a
;; `cons`-based tree of strings to be rendered through an inorder
;; traversal. The relevant syntax properties are 'raw, 'raw-prefix,
;; and 'raw-tail. The 'raw property on an atom is the original text
;; for the atom; whitespace and comments that precede the atom are
;; normally attached as 'raw-prefix. For a compound term, the
;; contained terms must be traversed to gather all the raw text; in
;; addition to 'raw-prefix and 'raw attached to the compound-term tag,
;; a 'raw-tail property holds raw text to be written after all of the
;; compound term's subterms. For example, the closing `)` for a
;; `parens` term will be attached to 'raw-tail on the 'parens tag. The
;; `group` and `top` tags may have non-empty 'raw-prefix and 'raw-tail
;; syntax properties, even though those tags don't have source
;; locations.

;; Parse all groups in a stream
(define (parse-top-groups l #:interactive? [interactive? #f])
  (define-values (gs rest-l end-line end-delta end-t tail-commenting tail-raw)
    (parse-groups l (make-group-state #:count? #t
                                      #:closer eof
                                      #:column #f
                                      #:check-column? #f
                                      #:last-line -1
                                      #:delta zero-delta)))
  (when tail-commenting (fail-no-comment-group tail-commenting))
  (unless (null? rest-l)
    (error "had leftover items" rest-l))
  (cond
    [(and interactive? (null? gs) (null? l)) eof]
    [else
     (define top (lists->syntax (cons top-tag (bars-insert-alts gs))))
     (add-raw-tail top tail-raw)]))

;; Parse a sequence of groups (top level, in opener-closer, or after `:`)
;;   consuming the closer in the case of opener-closer context.
;; Returns: the list of groups
;;          remaining tokens after a closer
;;          line of last consumed (possibly closer)
;;          delta of last consumed
;;          last token consumed (if a closer)
;;          pending group-comment token from end
;;          pending raw whitespace/comments
(define (parse-groups l sg)
  (define (next-line?* l line) (next-line? l line (group-state-count? sg)))
  (define (check-no-commenting #:tail-ok? [tail-ok? #f])
    (define commenting (or (group-state-commenting sg)
                           (and (not tail-ok?) (group-state-tail-commenting sg))))
    (when commenting
      (fail-no-comment-group commenting)))
  (define (done)
    (check-no-commenting #:tail-ok? #t)
    (values null l (group-state-last-line sg) (group-state-delta sg) #f
            (group-state-tail-commenting sg)
            (group-state-raw sg)))
  (define (check-column t column)
    (when (group-state-check-column? sg)
      (unless (column=? column (group-state-column sg)
                        #:incomparable (make-incomparable t))
        (fail t "wrong indentation"))))
  (define closer (group-state-closer sg))
  (cond
    [(null? l)
     ;; Out of tokens
     (when (string? (closer-expected closer))
       (fail (closer-expected-opener closer) (format "did not find matching ~s" (closer-expected closer))))
     (done)]
    [else
     (define t (car l))
     (define column (column+ (token-column t) (cont-delta-column (group-state-delta sg))))
     (define (less-indented?)
       (and (group-state-count? sg)
            (closer-column? closer)
            column
            (column . column<? . closer
                    #:incomparable (make-incomparable t))))
     (cond
       [(eq? (token-name t) 'group-comment)
        (check-no-commenting)
        ;; column doesn't have to match anything if it's on its own line
        (define line (token-line t))
        (define-values (rest-l last-line delta raw)
          (next-of (cdr l) line (group-state-delta sg) (cons t (group-state-raw sg))
                   (group-state-count? sg)))
        (define (continue-group sg)
          (parse-groups rest-l (struct-copy group-state sg
                                            [last-line last-line]
                                            [delta delta]
                                            [tail-commenting t]
                                            [raw raw])))
        (cond
          [(and (group-state-count? sg)
                line
                (line . > . (group-state-last-line sg))
                (next-line?* rest-l last-line))
           ;; on it's own line; doesn't affect indentation
           (continue-group sg)]
          [else
           ;; on the same line as group to comment out, so indentation matters
           (cond
             [(less-indented?) (done)]
             [else
              (check-column t column)
              (continue-group (struct-copy group-state sg
                                           [check-column? (next-line?* rest-l last-line)]
                                           [column column]
                                           [bar-column #f]))])])]
       [(less-indented?)
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
              (unless (equal? (closer-expected closer) (token-e t))
                (if (eof-object? (closer-expected closer))
                    (fail t (format "unexpected closer `~a`" (token-e t)))
                    (fail t (format "expected closer `~a`, found `~a`" (closer-expected closer) (token-e t)))))
              (define raw (cons t (group-state-raw sg)))
              (if (eof-object? (closer-expected closer))
                  ;; continue after extra closer:
                  (parse-groups (cdr l) (struct-copy group-state sg
                                                     [last-line (token-line t)]
                                                     [raw raw]))
                  ;; stop at closer
                  (begin
                    (check-no-commenting)
                    (values null (cdr l) (token-line t) (group-state-delta sg) t #f raw)))])]
          [(whitespace comment continue-operator)
           (define-values (next-l last-line delta raw)
             (next-of l (group-state-last-line sg) (group-state-delta sg) (group-state-raw sg)
                      (group-state-count? sg)))
           (parse-groups next-l (struct-copy group-state sg
                                             [last-line last-line]
                                             [delta delta]
                                             [raw raw]))]
          [(comma-operator)
           (cond
             [(closer-column? (group-state-closer sg))
              (done)]
             [else
              (unless (and (group-state-paren-immed sg)
                           (group-state-comma-time? sg))
                (fail t (format "misplaced comma~a"
                                (if (group-state-paren-immed sg)
                                    ""
                                    (string-append " (not immediately " within-parens-str ")")))))
              (define-values (rest-l last-line delta raw)
                (next-of (cdr l) (token-line t) (group-state-delta sg) (cons t (group-state-raw sg))
                         (group-state-count? sg)))
              (define next-line? (next-line?* rest-l last-line))
              (define bar-column
                (if (or next-line? (null? rest-l))
                    (group-state-column sg)
                    (column+ (token-column (car rest-l)) (cont-delta-column (group-state-delta sg)))))
              ;; In top level or immediately in opener-closer:
              (parse-groups rest-l (struct-copy group-state sg
                                                [check-column? next-line?]
                                                [bar-column bar-column]
                                                [last-line last-line]
                                                [comma-time? #f]
                                                [delta delta]
                                                [raw raw]))])]
          [(semicolon-operator)
           (cond
             [(eq? (group-state-block-mode sg) 'inside) (done)]
             [else
              (check-column t column)
              (define-values (rest-l last-line delta raw)
                (next-of (cdr l) (token-line t) (group-state-delta sg) (cons t (group-state-raw sg))
                         (group-state-count? sg)))
              (define next-t (and (pair? rest-l) (car rest-l)))
              (cond
                [(and next-t
                      (eq? 'opener (token-name next-t))
                      (equal? (token-e next-t) "«"))
                 (check-same-line t next-t (group-state-count? sg))
                 ;; group-sequence splice into the enclosing group
                 (define-values (group-commenting splice-l splice-last-line splice-delta splice-raw)
                   (next-of/commenting (cdr rest-l) last-line delta (cons next-t raw)
                                       (group-state-count? sg)))
                 (define-values (gs close-l close-line close-delta end-t never-tail-commenting group-tail-raw)
                   (parse-groups splice-l (make-group-state #:count? #f
                                                            #:closer (make-closer-expected "»" next-t)
                                                            #:paren-immed #f
                                                            #:block-mode #f
                                                            #:can-empty? #f
                                                            #:column #f
                                                            #:last-line splice-last-line
                                                            #:delta splice-delta
                                                            #:commenting group-commenting
                                                            #:raw splice-raw)))
                 (when (group-state-paren-immed sg)
                   (unless (and (pair? gs) (null? (cdr gs)))
                     (fail t (format "multi-group splice not allowed (immediately ~a)" within-parens-str))))
                 (define-values (more-gs more-l more-line more-delta more-end-t more-tail-commenting more-tail-raw)
                   (parse-groups close-l (struct-copy group-state sg
                                                      [comma-time? (and (group-state-paren-immed sg) #t)]
                                                      [check-column? (next-line?* close-l close-line)]
                                                      [column (or (group-state-column sg) column)]
                                                      [bar-column #f]
                                                      [last-line close-line]
                                                      [delta close-delta]
                                                      [commenting #f]
                                                      [tail-commenting #f]
                                                      [block-mode (next-group-block-mode (group-state-block-mode sg))]
                                                      [can-empty? #f]
                                                      [raw group-tail-raw])))
                 (values (append gs more-gs)
                         more-l more-line more-delta more-end-t more-tail-commenting more-tail-raw)]

                [else
                 (when (group-state-paren-immed sg)
                   (fail t (format "misplaced semicolon (immediately ~a)" within-parens-str)))
                 (parse-groups rest-l (struct-copy group-state sg
                                                   [check-column? (next-line?* rest-l last-line)]
                                                   [column (or (group-state-column sg) column)]
                                                   [bar-column #f]
                                                   [last-line last-line]
                                                   [delta delta]
                                                   [commenting (group-state-tail-commenting sg)]
                                                   [tail-commenting #f]
                                                   [block-mode (next-group-block-mode (group-state-block-mode sg))]
                                                   [raw raw]))])])]
          [else
           (when (and (group-state-comma-time? sg)
                      (not (eq? 'at (group-state-paren-immed sg))))
             (fail t (format "missing comma before new group (~a)" within-parens-str)))
           (case (and (not (eq? (group-state-block-mode sg) 'start))
                      (token-name t))
             [(bar-operator)
              (define line (token-line t))
              (cond
                [(or (group-state-bar-closes? sg)
                     (and line
                          (eqv? line (line+ (group-state-bar-closes-line sg)
                                            (cont-delta-line-span (group-state-delta sg))))))
                 (done)]
                [(or (eq? (group-state-block-mode sg) 'inside)
                     (and (in-block-mode? (group-state-block-mode sg))
                          (eq? 'block-operator (token-name (in-block-mode-token (group-state-block-mode sg))))))
                 ;; Bar at the start of a group
                 (define same-line? (or (not (group-state-count? sg))
                                        (not (group-state-last-line sg))
                                        (= line (group-state-last-line sg))))
                 (when (group-state-check-column? sg)
                   (unless (if (in-block-mode? (group-state-block-mode sg))
                               (column=? column (column-half-next (in-block-mode-parent-column (group-state-block-mode sg)))
                                         #:incomparable (make-incomparable t))
                               (or same-line?
                                   (column=? column (group-state-column sg)
                                             #:incomparable (make-incomparable t))))
                     (fail t "wrong indentation")))
                 (define pre-raw (group-state-raw sg))
                 (define commenting (or (group-state-commenting sg)
                                        (group-state-tail-commenting sg)))
                 (define-values (g rest-l group-end-line group-end-delta block-tail-commenting block-tail-raw)
                   (parse-block t (cdr l)
                                #:count? (group-state-count? sg)
                                #:line line
                                #:closer (or (column-next column) 'any)
                                #:bar-closes? (or (not (group-state-count? sg))
                                                  (not line))
                                #:bar-closes-line (and (group-state-count? sg)
                                                       (line+ line (- (cont-delta-line-span (group-state-delta sg)))))
                                #:delta (group-state-delta sg)
                                #:raw (if commenting
                                          null
                                          pre-raw)))
                 (define-values (gs rest-rest-l end-line end-delta end-t tail-commenting tail-raw)
                   (parse-groups rest-l (struct-copy group-state sg
                                                     [column (if same-line?
                                                                 (group-state-column sg)
                                                                 column)]
                                                     [bar-column #f]
                                                     [check-column? (next-line?* rest-l group-end-line)]
                                                     [last-line group-end-line]
                                                     [delta group-end-delta]
                                                     [comma-time? (and (group-state-paren-immed sg) #t)]
                                                     [commenting #f]
                                                     [tail-commenting block-tail-commenting]
                                                     [raw (if commenting
                                                              (append block-tail-raw
                                                                      (cons (syntax-to-raw (datum->syntax #f #`(group . #,g)))
                                                                            pre-raw))
                                                              block-tail-raw)])))
                 (values (if commenting
                             gs
                             (cons (list group-tag
                                         (add-span-srcloc
                                          t end-t
                                          ;; this 'bar will be converted by `tag-as-block`
                                          (cons 'bar g)))
                                   gs))
                         rest-rest-l
                         end-line
                         end-delta
                         end-t
                         tail-commenting
                         tail-raw)]
                [else
                 (if (and (in-block-mode? (group-state-block-mode sg))
                          (eq? 'block-operator (token-name (in-block-mode-token (group-state-block-mode sg)))))
                     (fail (in-block-mode-token (group-state-block-mode sg)) "unnecessary `:` before `|`")
                     (fail t "misplaced `|`"))])]
             [else
              ;; Parse one group, then recur to continue the sequence:
              (check-column t column)
              (define use-column (or (group-state-column sg) column))
              (define line (token-line t))
              (define pre-raw (group-state-raw sg))
              (define commenting (or (group-state-commenting sg)
                                     (group-state-tail-commenting sg)))
              (when (and (pair? l)
                         (eq? 'none (group-state-sequence-mode sg))
                         (not commenting))
                (fail t "second group not allowed after `@` within `«` and `»`"))
              (define-values (g rest-l group-end-line group-delta group-tail-commenting group-tail-raw)
                (parse-group l (make-state #:count? (group-state-count? sg)
                                           #:paren-immed (group-state-paren-immed sg)
                                           #:line line
                                           #:column use-column
                                           #:bar-column (group-state-bar-column sg)
                                           #:bar-closes? (group-state-bar-closes? sg)
                                           #:bar-closes-line (group-state-bar-closes-line sg)
                                           #:block-mode (group-state-block-mode sg)
                                           #:can-empty? (group-state-can-empty? sg)
                                           #:delta (group-state-delta sg)
                                           #:raw null)))
              (define-values (gs rest-rest-l end-line end-delta end-t tail-commenting tail-raw)
                (parse-groups rest-l (struct-copy group-state sg
                                                  [column use-column]
                                                  [check-column? (next-line?* rest-l group-end-line)]
                                                  [last-line group-end-line]
                                                  [comma-time? (and (group-state-paren-immed sg) #t)]
                                                  [delta group-delta]
                                                  [commenting #f]
                                                  [tail-commenting group-tail-commenting]
                                                  [raw (if commenting
                                                           (append group-tail-raw
                                                                   (cons (syntax-to-raw (datum->syntax #f #`(group . #,g)))
                                                                         pre-raw))
                                                           group-tail-raw)]
                                                  [sequence-mode (if (and (not commenting)
                                                                          (eq? 'one (group-state-sequence-mode sg)))
                                                                     'none
                                                                     (group-state-sequence-mode sg))])))
              (values (if commenting
                          gs
                          (cons (cons (add-pre-raw group-tag
                                                   pre-raw)
                                      g)
                                gs))
                      rest-rest-l
                      end-line
                      end-delta
                      end-t
                      tail-commenting
                      tail-raw)])])])]))

;; Parse one group.
;; Returns: the list of items in the group
;;          remaining tokens after group
;;          line of last consumed
;;          delta at last consumed
;;          pending group-comment token from end
(define (parse-group l s)
  (define (done)
    (values null l (state-line s) (state-delta s) #f (state-raw s)))
  (cond
    [(null? l) (done)]
    [else
     (define t (car l))
     (define line (token-line t))
     (define (check-block-mode)
       (when (eq? (state-block-mode s) 'end)
         (fail t "no terms allowed after `»` within a group")))
     ;; Consume a token
     (define (keep delta
                   #:operator-column [operator-column (state-operator-column s)]
                   #:at-mode [at-mode (state-at-mode s)]
                   #:suffix? [suffix? #t])
       (check-block-mode)
       ;; a `literal` as an S-expression) can span lines, so we check
       ;; the next token's start to decide the token's ending line
       (define post-line (if (pair? (cdr l))
                             (token-line (cadr l))
                             line))
       (define-values (suffix-raw suffix-l suffix-line suffix-delta)
         (if suffix?
             (get-suffix-comments (cdr l) post-line delta)
             (values null (cdr l) post-line delta)))
       (define-values (at-adjust new-at-mode at-l at-line at-delta)
         (continue-at at-mode #f suffix-l suffix-line suffix-delta (state-count? s)))
       (define-values (g rest-l end-line end-delta tail-commenting tail-raw)
         (parse-group at-l (struct-copy state s
                                        [line at-line]
                                        [delta at-delta]
                                        [raw null]
                                        [block-mode (next-block-mode (state-block-mode s))]
                                        [can-empty? #f]
                                        [operator-column operator-column]
                                        [at-mode new-at-mode])))
       (define elem (record-raw (token-value t) #f (state-raw s) suffix-raw))
       (values (at-adjust (cons elem g)) rest-l end-line end-delta tail-commenting tail-raw))
     (define (parse-alts-block t l
                               #:group-commenting [group-commenting #f]
                               #:delta [delta (state-delta s)]
                               #:raw [raw (state-raw s)])
       (define line (token-line t))
       (cond
         [(or (state-bar-closes? s)
              (and line
                   (eqv? line (line+ (state-bar-closes-line s)
                                     (cont-delta-line-span (state-delta s))))))
          (done)]
         [else
          ;; no `(check-block-mode)` here, because `|` is allowed after `:`
          (when (and (state-operator-column s)
                     (not (column=? (token-column t) (column-half-next (state-operator-column s))
                                    #:incomparable (make-incomparable t))))
            (fail t "wrong indentation"))
          (parse-block #f l
                       #:count? (state-count? s)
                       #:block-mode 'inside
                       #:line line
                       #:closer (or (and (state-count? s) (token-column t)) 'any)
                       #:bar-closes? #f
                       #:bar-closes-line #f
                       #:drop-empty? #t ;; possible when `commenting`
                       #:delta delta
                       #:raw raw
                       #:group-commenting group-commenting)]))
     ;; Dispatch
     (cond
       [(and (state-count? s)
             line
             (line . > . (state-line s)))
        ;; new line
        (case (token-name t)
          [(whitespace comment)
           (parse-group (cdr l) (struct-copy state s
                                             [line line]
                                             [delta zero-delta]
                                             [raw (cons t (state-raw s))]))]
          [(closer at-content at-closer)
           (done)]
          [else
           ;; consume any group comments that are on their own line or prefixing `|`:
           (define-values (group-commenting use-t use-l last-line delta raw)
             (get-own-line-group-comment t l (state-line s) (state-delta s) (state-raw s) (state-count? s)))
           (define column (token-column use-t))
           (cond
             [(column . column>? . (state-column s)
                      #:incomparable (make-incomparable use-t))
              ;; More indented forms a nested block when there's
              ;; a preceding `:` (doesn't get here) or starting with `|`;
              ;; more indented continues a group when starting with an
              ;; operator
              (cond
                [(eq? 'bar-operator (token-name use-t))
                 (unless (column=? column (column-half-next (or (state-bar-column s)
                                                                (state-column s)))
                                   #:incomparable (make-incomparable use-t))
                   (fail use-t "wrong indentation"))
                 (parse-block #f use-l
                              #:count? (state-count? s)
                              #:block-mode 'inside
                              #:line (token-line use-t)
                              #:closer (or column 'any)
                              #:bar-closes? #f
                              #:bar-closes-line #f
                              #:drop-empty? #t ;; possible when `group-commenting`
                              #:delta delta
                              #:raw raw
                              #:group-commenting group-commenting)]
                [(and (eq? 'operator (token-name use-t))
                      (or (not (state-operator-column s))
                          (column=? column (state-operator-column s)
                                    #:incomparable (make-incomparable use-t))))
                 (when group-commenting (fail group-commenting "misplaced group comment"))
                 (keep zero-delta #:operator-column column)]
                [(and (eq? 'opener (token-name use-t))
                      (equal? "«" (token-e t)))
                 (when group-commenting (fail group-commenting "misplaced group comment"))
                 ;; will error as out-of-place
                 (parse-group l (struct-copy state s
                                             [line line]))]
                [else
                 (fail use-t "wrong indentation (or missing `:` on previous line)")])]
             [else
              ;; using `done` here means that we leave any group comments in place;
              ;; not consuming inspected tokens is normally worrisome, but we'll parse
              ;; them at most one more time
              (done)])])]
       [else
        ;; Not a new line
        (case (token-name t)
          [(closer comma-operator semicolon-operator at-content at-closer)
           (done)]
          [(identifier number literal operator)
           (keep (state-delta s))]
          [(block-operator)
           (check-block-mode)
           (parse-block t (cdr l)
                        #:count? (state-count? s)
                        #:line line
                        #:closer (or (and (state-count? s)
                                          (column-half-next (state-column s)))
                                     'any)
                        #:delta (state-delta s)
                        #:raw (state-raw s)
                        #:bar-closes? (and (state-bar-closes? s)
                                           (not (state-bar-closes-line s)))
                        #:bar-closes-line (state-bar-closes-line s)
                        #:can-empty? (state-can-empty? s)
                        #:could-empty-if-start? #t
                        #:parent-column (state-column s))]
          [(bar-operator)
           (parse-alts-block t l)]
          [(opener)
           (check-block-mode)
           (define-values (closer tag paren-immed)
             (case (token-e t)
               [("(") (values ")" 'parens (let ([am (state-at-mode s)])
                                            (if (and am (not (at-mode-initial? am)))
                                                'at
                                                'normal)))]
               [("{") (values "}" 'braces 'normal)]
               [("[") (values "]" 'brackets 'normal)]
               [("'") (values "'" 'quotes #f)]
               [("«") (define am (state-at-mode s))
                      (if (and am (at-mode-initial? am))
                          (values "»" 'at #t)
                          (fail t "misplaced `«`"))]
               [else (error "unknown opener" t)]))
           (define pre-raw (state-raw s))
           (define-values (group-commenting next-l0 last-line0 delta0 raw0)
             (next-of/commenting (cdr l) line (state-delta s) null (state-count? s)))
           (define-values (next-l last-line delta raw use-closer quote-nested?)
             (cond
               [(and (eq? tag 'quotes) (pair? next-l0) (not group-commenting)
                     (eq? 'opener (token-name (car next-l0)))
                     (equal? "«" (token-e (car next-l0))))
                (define-values (next-l last-line delta raw)
                  (next-of (cdr next-l0) last-line0 delta0 (cons (car next-l0) raw0) (state-count? s)))
                (values next-l last-line delta raw "»" #t)]
               [else
                (values next-l0 last-line0 delta0 raw0 closer #f)]))
           (define sub-column
             (if (pair? next-l)
                 (column+ (token-column (car next-l)) (cont-delta-column (state-delta s)))
                 (column-next (column+ (token-column t) (cont-delta-column (state-delta s))))))
           (define-values (gs rest-l0 close-line0 close-delta0 end-t0 never-tail-commenting group-tail-raw0)
             (parse-groups next-l (make-group-state #:count? (state-count? s)
                                                    #:closer (make-closer-expected use-closer t)
                                                    #:paren-immed paren-immed
                                                    #:block-mode 'start
                                                    #:column sub-column
                                                    #:last-line last-line
                                                    #:delta delta
                                                    #:commenting group-commenting
                                                    #:raw raw
                                                    #:sequence-mode (if (eq? tag 'at) 'one 'any))))
           (define-values (rest-l close-line close-delta end-t group-tail-raw)
             (cond
               [quote-nested?
                (define-values (next-l last-line delta raw)
                  (next-of rest-l0 close-line0 close-delta0 group-tail-raw0 (state-count? s)))
                (cond
                  [(and (pair? next-l)
                        (eq? 'closer (token-name (car next-l)))
                        (equal? "'" (token-e (car next-l))))
                   (values (cdr next-l) last-line delta (car next-l) (cons (car next-l) raw))]
                  [else
                   (fail end-t0 (format "expected closing \"'\" after closing \"»\""))
                   (values next-l last-line delta end-t0 raw)])]
               [(and (state-at-mode s) (at-mode-splice? (state-at-mode s)))
                (define post-t (if (pair? rest-l0) (car rest-l0) end-t0))
                (define rest-rest-l (if (pair? rest-l0) (cdr rest-l0) rest-l0))
                (unless (and (eq? 'closer (token-name post-t))
                             (equal? ")" (token-e post-t)))
                  (fail end-t0 "expected a closing `)` immediately after closing `»`"))
                (values rest-rest-l close-line0 close-delta0 post-t (cons post-t group-tail-raw0))]
               [else
                (values rest-l0 close-line0 close-delta0 end-t0 group-tail-raw0)]))
           (define-values (suffix-raw suffix-l suffix-line suffix-delta)
             (get-suffix-comments rest-l close-line close-delta))
           (define-values (at-adjust new-at-mode at-l at-line at-delta)
             (continue-at (state-at-mode s) (equal? closer ")") suffix-l suffix-line suffix-delta (state-count? s)))
           (define-values (g rest-rest-l end-line end-delta tail-commenting tail-raw)
             (parse-group at-l (struct-copy state s
                                            [line at-line]
                                            [delta at-delta]
                                            [block-mode (next-block-mode (state-block-mode s))]
                                            [raw null]
                                            [at-mode new-at-mode])))
           (define new-g (at-adjust
                          (cons (add-raw-to-prefix
                                 t pre-raw #:tail group-tail-raw #:tail-suffix (reverse suffix-raw)
                                 (add-span-srcloc
                                  t end-t
                                  (cons tag gs)))
                                g)))
           (define-values (result-g result-tail-raw)
             (if (eq? tag 'at)
                 (splice-at t new-g tail-raw)
                 (values new-g tail-raw)))
           (values result-g
                   rest-rest-l
                   end-line
                   end-delta
                   tail-commenting
                   result-tail-raw)]
          [(whitespace comment continue-operator)
           (define-values (next-l line delta raw)
             (next-of l (state-line s) (state-delta s) (state-raw s) (state-count? s)))
           (parse-group next-l (struct-copy state s
                                            [line line]
                                            [delta delta]
                                            [raw raw]))]
          [(group-comment)
           ;; misplaced, unless it's commenting out a `|` on the same line
           (define-values (next-l line delta raw)
             (next-of (cdr l) (token-line t) (state-delta s) (cons t (state-raw s)) (state-count? s)))
           (cond
             [(and (pair? next-l)
                   (eq? (token-name (car next-l)) 'bar-operator)
                   (not (next-line? next-l line (state-count? s))))
              (parse-alts-block (car next-l) next-l
                                #:group-commenting t
                                #:delta delta
                                #:raw raw)]
             [else
              (fail t "misplaced group comment")
              (parse-group (cdr l) s)])]
          [(at)
           (check-block-mode)
           (define am (state-at-mode s))
           (cond
             [(and am
                   (at-mode-stop-at-at? am))
              (done)]
             [(null? (cdr l))
              (fail t "missing term after `@`")
              (parse-group null s)]
             [else
              (define next-t (cadr l))
              (check-same-line t next-t (state-count? s))
              (define stop-at-next-at? (and am (at-mode-stop-at-next-at? am)))
              (case (token-name next-t)
                [(opener)
                 (define (normal-opener)
                   (parse-group (cdr l) (struct-copy state s
                                                     [raw (cons t (state-raw s))]
                                                     [at-mode (make-at-mode #:initial? #t
                                                                            #:stop-at-at? stop-at-next-at?)])))
                 (case (token-e next-t)
                   [("(")
                    (cond
                      [(and (pair? (cddr l))
                            (let ([next-next-t (caddr l)])
                              (and (eq? 'opener (token-name next-next-t))
                                   (equal? "«" (token-e next-next-t)))))
                       ;; A `@(« ... »)` escape does not support arguments, while its
                       ;; content is spliced like `@« ... »`; we drop the `(` here,
                       ;; and the `»` parser will consume the `)`
                       (parse-group (cddr l) (struct-copy state s
                                                          [raw (list* next-t t (state-raw s))]
                                                          [at-mode (make-at-mode #:initial? #t
                                                                                 #:splice? #t
                                                                                 #:stop-at-at? stop-at-next-at?)]))]
                      [else
                       (normal-opener)])]
                   [("[" "«" "'") (normal-opener)]
                   [else (error "unexpected" (token-name next-t)  (token-e next-t))])]
                [(identifier)
                 ;; handle <identifier> <operator> ... <identifier> with no spaces in between
                 (let loop ([l (cdr l)] [rev-prefix '()] [raw (cons t (state-raw s))])
                   (cond
                     [(and (pair? (cdr l))
                           (pair? (cddr l))
                           (eq? 'operator (token-name (cadr l)))
                           (eq? 'identifier (token-name (caddr l))))
                      (define id (record-raw (token-value (car l)) #f raw '()))
                      (define dot (record-raw (token-value (cadr l)) #f '() '()))
                      (loop (cddr l) (list* dot id rev-prefix) '())]
                     [else
                      (parse-group l (struct-copy state s
                                                  [raw raw]
                                                  [at-mode (make-at-mode #:initial? #t
                                                                         #:rev-prefix rev-prefix
                                                                         #:stop-at-at? stop-at-next-at?)]))]))]
                [(number literal operator opener)
                 (parse-group (cdr l) (struct-copy state s
                                                   [raw (cons t (state-raw s))]
                                                   [at-mode (make-at-mode #:initial? #t
                                                                          #:stop-at-at? stop-at-next-at?)]))]
                [(at-opener)
                 (keep (state-delta s) #:at-mode (make-at-mode #:stop-at-at? stop-at-next-at?) #:suffix? #f)]
                [(whitespace)
                 (fail next-t "whitespace is invalid after `@`")]
                [else
                 (fail next-t "invalid after `@`")])])]
          [(at-comment)
           (fail t "comments using `@//` are allowed only within `@` body")]
          [else
           (error "unexpected" (token-value t))])])]))

(define (parse-block t l
                     #:count? count?
                     #:line line
                     #:closer closer
                     #:bar-closes? [bar-closes? #f]
                     #:bar-closes-line [bar-closes-line #f]
                     #:drop-empty? [drop-empty? #f]
                     #:delta in-delta
                     #:raw in-raw
                     #:group-commenting [in-group-commenting #f]
                     #:parent-column [parent-column +inf.0]
                     #:block-mode [block-mode (in-block-mode t parent-column)]
                     #:can-empty? [can-empty? #f]
                     #:could-empty-if-start? [could-empty-if-start? #f])
  (define-values (opener-t opener-l opener-line opener-delta opener-raw)
    (next-of/opener l line in-delta null count?))
  (when opener-t
    (check-same-line t opener-t count?))
  (define inside-count? (and count? (not opener-t)))
  (define-values (group-commenting next-l last-line delta raw)
    (if in-group-commenting
        (values in-group-commenting opener-l opener-line opener-delta opener-raw)
        (next-of/commenting opener-l opener-line opener-delta opener-raw inside-count?)))
  (define (fail-empty)
    (fail t (format "empty block not allowed after `~a` (except with `«` and `»`~a)"
                    (token-e t)
                    (if could-empty-if-start?
                        ", at top, or in an opener-closer pair"
                        ""))))
  (cond
    [(pair? next-l)
     (define next-t (car next-l))
     (define content-column (column+ (token-column next-t) (cont-delta-column delta)))
     (define-values (indent-gs rest-l end-line end-delta end-t tail-commenting tail-and-suffix-raw)
       (parse-groups next-l
                     (make-group-state #:count? inside-count?
                                       #:closer (if opener-t
                                                    (make-closer-expected "»" opener-t)
                                                    closer)
                                       #:column content-column
                                       #:last-line last-line
                                       #:bar-closes? (and (not opener-t) bar-closes?)
                                       #:bar-closes-line (and (not opener-t) inside-count? bar-closes-line)
                                       #:block-mode block-mode
                                       #:can-empty? #f
                                       #:delta delta
                                       #:commenting group-commenting
                                       #:raw raw)))
     (when (and (not can-empty?) (null? indent-gs) t (not opener-t))
       (fail-empty))
     (define-values (tail-raw rev-suffix-raw)
       ;; potential (but unlikely) quadratric behavior here; see
       ;; `keep-same-indentation-comments` for more information
       (keep-same-indentation-comments content-column tail-and-suffix-raw))
     (define used-closer? (or opener-t
                              (closer-expected? closer)))
     (define-values (post-g post-l post-line post-delta post-tail-commenting post-tail-raw)
       (if used-closer?
           ;; in 'end mode, so errors, returns a null group, or returns alts:
           (parse-group rest-l (make-state #:count? count?
                                           #:line end-line
                                           #:column parent-column
                                           #:bar-closes? bar-closes?
                                           #:bar-closes-line bar-closes-line
                                           #:block-mode 'end
                                           #:delta end-delta
                                           #:raw null))
           (values '() rest-l end-line end-delta tail-commenting (if used-closer?
                                                                     null
                                                                     tail-raw))))
     (unless (or (null? post-g) (null? (cdr post-g)) (eq? 'alts (syntax-e (caar post-g)))) (error "internal error, parsed more"))
     (define-values (head-block post-alts) (tag-as-block indent-gs))
     (values (append
              (if (and (null? indent-gs)
                       drop-empty?)
                  null
                  (cons (add-raw-to-prefix
                         t in-raw #:tail (let ([tail-raw (and used-closer? tail-raw)])
                                           (if (pair? rev-suffix-raw)
                                               (append (or tail-raw null) rev-suffix-raw)
                                               tail-raw))
                         (add-span-srcloc
                          t end-t #:alt next-t
                          head-block))
                        post-alts))
              post-g)
             post-l
             post-line
             post-delta
             post-tail-commenting
             (if (and (null? indent-gs)
                      drop-empty?)
                 (append post-tail-raw in-raw)
                 post-tail-raw))]
    [else
     (when opener-t (fail opener-t (format "expected `»`")))
     (when (and (not can-empty?) t (not opener-t)) (fail-empty))
     (define-values (head-block post-alts) (tag-as-block null))
     (values (list (add-raw-to-prefix
                    t in-raw
                    (add-span-srcloc
                     t #f
                     head-block)))
             next-l
             line
             delta
             group-commenting
             raw)]))

;; converts to `block` and/or `alts`
(define (tag-as-block gs)
  (let loop ([gs gs] [accum null])
    (cond
      [(and (pair? gs)
            ;; really only need to check the first one:
            (for/and ([g (in-list gs)])
              (and (pair? g)
                   (tag? 'group (car g))
                   (pair? (cdr g))
                   (null? (cddr g))
                   (let ([b (cadr g)])
                     (and (pair? b)
                          (tag? 'bar (car b))
                          ;; the rest should always be true:
                          (pair? (cdr b))
                          (null? (cddr b))
                          (pair? (cadr b))
                          (tag? 'block (caadr b)))))))
       (define end-alts
         (cons alts-tag
               (for/list ([g (in-list gs)])
                 (move-pre-raw
                  (car g)
                  (let ([b (cadr g)])
                    (cadr b))))))
       (if (null? accum)
           (values end-alts null)
           (values (cons 'block (reverse accum))
                   (list end-alts)))]
      [(null? gs) (values (cons 'block (reverse accum))
                          null)]
      [else (loop (cdr gs) (cons (car gs) accum))])))

(define (bars-insert-alts gs)
  (define-values (head-block tail-alts) (tag-as-block gs))
  (append (cdr head-block) tail-alts))

(define (tag? sym e)
  (or (eq? sym e)
      (and (syntax? e)
           (eq? sym (syntax-e e)))))

;; Look for `{` (as 'at-opener) next or a `(` that might be followed
;; by a `{`, and prepare to convert by rearranging info a splice
;; followed by parentheses
(define (continue-at am after-paren? l line delta count?)
  (define (at-call rator parens g)
    (cond
      [(not (at-mode-initial? am))
       (cons (move-pre-raw rator
                           (add-raw-to-prefix #f (list (syntax-to-raw rator)) parens))
             g)]
      [(pair? (at-mode-rev-prefix am))
       (append (reverse (cons rator (at-mode-rev-prefix am))) (cons parens g))]
      [else
       (list* rator parens g)]))
  (define (keep-stop-mode am)
    (make-at-mode #:stop-at-at? (at-mode-stop-at-at? am)))
  (cond
    [(not am)
     (values (lambda (g) g) #f l line delta)]
    [(and (or (not after-paren?)
              (at-mode-initial? am))
          (pair? l)
          (eq? 'opener (token-name (car l)))
          (let ([s (token-e (car l))])
            (or (string=? "(" s)
                (string=? "[" s))))
     (when (string=? "[" (token-e (car l)))
       (fail (car l) "argument position for `@` cannot start `[`"))
     (values (lambda (g)
               (define a (cadr g))
               (define tag (car a))
               (cond
                 [(tag? 'parens tag)
                  (at-call (car g)
                           a
                           (cddr g))]
                 ;; can these other cases happen?
                 [(not (at-mode-initial? am))
                  (add-raw-to-prefix* #f (list (syntax-to-raw (car g)))
                                      (cdr g))]
                 [(pair? (at-mode-rev-prefix am))
                  (append (at-mode-rev-prefix am) g)]
                 [else g]))
             (keep-stop-mode am)
             l line delta)]
    [(and (pair? l)
          (eq? 'at-opener (token-name (car l))))
     ;; process a `{`...`}` body, handling escapes and then trimming whitespace
     (define init-t (car l))
     (let loop ([l (cdr l)] [accum-args '()])
       (parse-text-sequence
        l
        line delta
        #:opener-t init-t
        #:count? count?
        (lambda (seq l line delta)
          (define c (list group-tag seq))
          (cond
            [(and (pair? l) (pair? (cdr l)) (eq? 'at-opener (token-name (cadr l))))
             ;; more `{}` arguments
             (loop (cddr l) (cons c accum-args))]
            [else
             (values (lambda (g)
                       (cond
                         [(or (not after-paren?)
                              (at-mode-initial? am)
                              (pair? at-mode))
                          (at-call (car g)
                                   (cons parens-tag (reverse (cons c accum-args)))
                                   (cdr g))]
                         [else
                          (define bracket (caar g))
                          (define new-g (cons (cons parens-tag
                                                    (append
                                                     (cdar g)
                                                     (let ([args (reverse (cons c accum-args))])
                                                       (cons
                                                        (move-post-raw-to-prefix bracket (car args))
                                                        (cdr args)))))
                                              (cdr g)))
                          (move-pre-raw bracket
                                        (add-raw-to-prefix* #f (list (syntax-raw-property bracket))
                                                            new-g))]))
                     (keep-stop-mode am)
                     (if (null? l) null (cdr l)) line delta)]))))]
    [else
     (values (lambda (g)
               (append
                (reverse (at-mode-rev-prefix am))
                (if (pair? at-mode)
                    (append (reverse at-mode) g)
                    g)))
             (and (at-mode-stop-at-at? am)
                  (make-at-mode #:stop-at-at? #t))
             l line delta)]))

(define (parse-text-sequence l line delta
                             done-k
                             #:opener-t [opener-t #f]
                             #:count? [count? #t])
  (let loop ([l l] [content '()])
    (case (if (null? l) 'at-closer (token-name (car l)))
      [(at-closer)
       (when (and (null? l) opener-t)
         (fail opener-t "missing closer for `@` content"))
       (define-values (prefix-syntaxes new-content post-syntaxes)
         (adjust-content-space content group-tag))
       (define seq
         (add-tail-raw-to-prefix
          (if (null? l)
              null
              (list (car l)))
          post-syntaxes
          (let ([tag (if opener-t
                         (syntax-property
                          (datum->syntax (token-value opener-t)
                                         'brackets
                                         (token-value opener-t)
                                         (token-value opener-t))
                          'identifier-as-keyword #t)
                         brackets-tag)])
            (cond
              [(null? new-content) (list tag)]
              [else (cons tag
                          (add-raw-to-prefix* #f (map syntax-to-raw prefix-syntaxes)
                                              new-content))]))))
       (define end-line (if (pair? l) (token-line (car l)) line))
       (done-k seq l end-line (if (eqv? line end-line) delta zero-delta))]
      [(at-content)
       (loop (cdr l)
             ;; mark as 'content instead of 'group for now, so we
             ;; can split and trim whitespace after finding all of it
             (cons (list 'content (token-value (car l)))
                   content))]
      [(at at-comment)
       (define t (car l))
       (define comment? (eq? (token-name t) 'at-comment))
       ;; `parse-group` work will be delimited by 'at-content, 'at-closer, or another 'at
       (define-values (g rest-l group-end-line group-delta group-tail-commenting group-tail-raw)
         (parse-group (if comment?
                          (cons (token-rename t 'at) (cdr l))
                          l)
                      (make-state #:count? count?
                                  #:line (token-line t)
                                  #:column (token-column t)
                                  #:delta zero-delta
                                  #:at-mode (make-at-mode #:initial? #t
                                                          #:stop-at-next-at? #t)
                                  #:raw null)))
       (loop rest-l
             (cons (if comment?
                       (list 'comment (cons (token-raw t) (syntax-to-raw #`(group . #,g))))
                       (cons group-tag g))
                   content))]
      [(comment)
       (loop (cdr l) (cons (list 'comment (token-e (car l))) content))]
      [else (error "unexpected in at" (token-name (car l)))])))

(define (splice-at t g tail-raw)
  (define gs (car g))
  (define at (car gs))
  (unless (tag? 'at at) (error "expected at"))
  (when (null? (cdr gs)) (fail t "empty group after `@` within `«` and `»`"))
  (unless (null? (cddr gs)) (error "extra groups in at (should be caught earlier)"))
  (define rest (cdr g))
  (unless (null? rest)
    (let loop ([gs (cdadr gs)])
      (cond
        [(null? gs) (void)]
        [(null? (cdr gs))
         (define term (car gs))
         (define tag (and (pair? term) (syntax-e (car term))))
         (when (or (eq? tag 'block) (eq? tag 'alts))
           (fail t "block not allowed after mid-group `@` within `«` and `»`"))]
        [else (loop (cdr gs))])))
  (values
   (move-pre-raw* at
                  (add-raw-to-prefix* #f (list (syntax-raw-property at))
                                      (append (cdadr gs)
                                              (if (null? rest)
                                                  rest
                                                  (move-post-raw-to-prefix* at rest)))))
   (if (null? rest)
       (append tail-raw (list (or (syntax-raw-tail-property at) '())))
       tail-raw)))

;; Like `datum->syntax`, but sytheses a source location for each
;; nested list:
;;
;; * If the list is for a group or alts, then a spanning source
;;   location is computed.
;;
;; * Otherwise, propagates the source location of a start of a list
;;   to the list itself, where that starting item is expected
;;   to be a tag that has the span of the whole term already as its
;;   location.
;;
(define (lists->syntax l)
  (cond
    [(pair? l)
     (define a (car l))
     (define new-l (for/list ([e (in-list (cdr l))])
                     (lists->syntax e)))
     (define tag (syntax-e a))
     (cond
       [(or (eq? tag 'group) (eq? tag 'alts))
        ;; compute span, given that group and alts cannot be empty
        (datum->syntax* #f
                        (cons a new-l)
                        (srcloc-spanning (syntax-srcloc (car new-l))
                                         (syntax-srcloc
                                          (let loop ([new-l new-l])
                                            (if (null? (cdr new-l))
                                                (car new-l)
                                                (loop (cdr new-l))))))
                        stx-for-original-property)]
       [else
        (datum->syntax* #f (cons a new-l) a stx-for-original-property)])]
    [else l]))

;; Consume whitespace and comments, including continuing backslashes,
;; where lookahead is needed
;; Arguments/returns:
;;   list of input tokens (after whitespace, comments, and continues)
;;   most recently consumed non-whitesace line (to determine whether
;;     next is on same line); on input; the line can be #f, which means
;;     "treat as same line"; the result is never #f
;;   current line delta (created by continues)
;;   accumulated reversed raw whitespace
(define (next-of l last-line delta raw count?)
  (cond
    [(null? l) (values null (or last-line 0) (or delta 0) raw)]
    [else
     (define t (car l))
     (case (token-name t)
       [(whitespace comment)
        (next-of (cdr l) last-line delta (cons (car l) raw) count?)]
       [(continue-operator)
        (define line (token-line t))
        ;; a continue operator not followed only by whitespace and
        ;; comments is an error
        (define-values (next-l next-raw)
          (let loop ([l (cdr l)] [raw (cons t raw)])
            (cond
              [(null? l) (values null raw)]
              [else (case (token-name (car l))
                      [(whitespace comment) (loop (cdr l) (cons (car l) raw))]
                      [else (values l raw)])])))
        (cond
          [(and (pair? next-l)
                (or (not count?)
                    (eqv? line (token-line (car next-l)))))
           (when count?
             (fail t "line-continuing '\\' is followed by a another token on the same line"))
           ;; if error escape or counting is disabled, threat like whitespace:
           (next-of next-l last-line delta next-raw count?)]
          [else
           (define continues? (or (not count?) (not last-line) (eqv? line last-line)))
           (next-of next-l
                    (if continues?
                        ;; whitespace-only lines don't count, so next continues
                        ;; on the same line by definition:
                        #f
                        last-line)
                    (cont-delta 0
                                #;
                                (column+ (column+ 1
                                                  (token-column t))
                                         (if continues?
                                             (cont-delta-column delta)
                                             0))
                                (add1 (cont-delta-line-span delta)))
                    next-raw
                    count?)])]
       [else
        (define line (token-line t))
        (values l
                (or last-line line)
                (if (or (not count?) (not last-line) (eqv? line last-line))
                    delta
                    zero-delta)
                raw)])]))

(define (next-of/commenting l last-line delta raw count?)
  (define-values (rest-l rest-last-line rest-delta rest-raw)
    (next-of l last-line delta raw count?))
  (cond
    [(pair? rest-l)
     (define-values (group-commenting use-t use-l last-line delta raw)
       (get-own-line-group-comment (car rest-l) rest-l rest-last-line rest-delta rest-raw count?))
     (values group-commenting use-l last-line delta raw)]
    [else
     (values #f rest-l rest-last-line rest-delta rest-raw)]))

;; skip whitespace, but also gather a group comment that is
;; on its own line, so it doesn't influence indentation
(define (next-of/opener l last-line delta raw count?)
  (define-values (rest-l rest-last-line rest-delta rest-raw)
    (next-of l last-line delta raw count?))
  (cond
    [(pair? rest-l)
     (define t (car rest-l))
     (cond
       [(and (eq? (token-name t) 'opener)
             (equal? (token-e t) "«"))
        (values t (cdr rest-l) (token-line t) rest-delta (cons t rest-raw))]
       [else
        (values #f rest-l rest-last-line rest-delta rest-raw)])]
    [else
     (values #f rest-l rest-last-line rest-delta rest-raw)]))

;; t is at the start of l on input and output
(define (get-own-line-group-comment t l line delta raw count?)
  (let loop ([commenting #f] [t t] [l (cdr l)] [line line] [delta delta] [raw raw])
    (case (token-name t)
      [(group-comment)
       (when commenting (fail-no-comment-group commenting))
       (define (not-on-own-line)
         ;; not on own line, so leave in stream to influence indentation
         (values commenting t (cons t l) line delta raw))
       (cond
         [(and count? ((token-line t) . > . line))
          (define-values (next-l last-line next-delta next-raw)
            (next-of l line delta (cons t raw) count?))
          (cond
            [(null? next-l) (fail-no-comment-group t)]
            [(next-line? next-l (token-line t) count?)
             (loop t (car next-l) (cdr next-l) last-line next-delta next-raw)]
            [else
             ;; note: discading work to skip whitespace, but we should
             ;; performs that work again at most once
             (not-on-own-line)])]
         [else
          (not-on-own-line)])]
      [else
       (values commenting t (cons t l) line delta raw)])))

;; consumes whitespace and comments that stay on the same line; we
;; check the whitespace and comment content for "\n" to decide whether
;; it stays on the same line
(define (get-suffix-comments l line delta)
  (cond
    [(null? l) (values null null line delta)]
    [else
     (define t (car l))
     (case (token-name t)
       [(whitespace comment)
        (cond
          [(regexp-match? #rx"\n" (token-e t))
           (values null l line delta)]
          [else
           ;; consume suffix comment
           (define-values (raw rest-l rest-line rest-delta)
             (get-suffix-comments (cdr l) line delta))
           (values (cons (car l) raw) rest-l rest-line rest-delta)])]
       [else
        (values null l line delta)])]))

;; extract a prefix of the reversed `tail-raw` where comments
;; line up with `column`; since we reverse `tail-raw` and
;; reverse is back, there's a potentially of O(N^2) behavior
;; if blocks are nested N deep with N comments afterwards --- but
;; that would be an unusual source file, so we don't worry about
;; it for now
(define (keep-same-indentation-comments column tail-raw)
  (cond
    [(not column) (values tail-raw null)]
    [else
     (let loop ([raw (reverse tail-raw)] [pending null] [rev-suffix-raw null])
       (define (done)
         (values (reverse (append (reverse pending) raw)) rev-suffix-raw))
       (cond
         [(null? raw) (done)]
         [else
          (define t (car raw))
          (case (and (token? t) (token-name t))
            [(whitespace)
             (loop (cdr raw) (cons t pending) rev-suffix-raw)]
            [(comment)
             (cond
               [(column=? column (token-column t)
                          #:incomparable (make-incomparable t))
                (loop (cdr raw) null (cons t (append pending rev-suffix-raw)))]
               [else (done)])]
            [else (done)])]))]))

(define (next-line? l last-line count?)
  (and count?
       (pair? l)
       (let ([line (token-line (car l))])
         (and line
              (line . > . last-line)))))

(define (check-same-line t next-t count?)
  (when count?
    (unless (eqv? (token-line t) (token-line next-t))
      (fail next-t (format "not on the same line as preceding `~a`"
                           (token-e t))))))

(define (fail-no-comment-group t)
  (fail t "no group for term comment"))

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
  (and c
       (cond
         [(integer? c) (add1 c)]
         [(number? c) (add1 (inexact->exact (floor c)))]
         [else (cons (column-next (car c)) (cdr c))])))

(define (column-half-next c)
  (cond
    [(integer? c)
     (+ c 0.5)]
    [(pair? c) (cons (column-half-next (car c))
                     (cdr c))]
    [else
     (column-next c)]))

(define (column+ c n)
  (and c (lex:column+ c n)))

(define (line+ l n)
  (and l (+ l n)))

(define (next-block-mode mode)
  #f)

(define (next-group-block-mode mode)
  (cond
    [(in-block-mode? mode)
     ;; keep block mode to allow a `|` continuation afterward
     mode]
    [else #f]))

;; ----------------------------------------

(define (add-span-srcloc start-t end-t l
                         #:alt [alt-start-t #f])
  (define (add-srcloc l s-loc loc)
    (cons (let* ([stx (datum->syntax* #f (car l) loc stx-for-identifier-as-keyword)])
            (if (syntax? start-t)
                (let* ([stx (syntax-property-copy stx start-t syntax-raw-property)]
                       [stx (syntax-property-copy stx start-t syntax-raw-prefix-property)]
                       [stx (syntax-property-copy stx start-t syntax-raw-suffix-property)])
                  stx)
                stx))
          (cdr l)))
  (define last-t/e (or end-t
                       ;; when `end-t` is false, we go looking for the
                       ;; end in `l`; this search walks down the end
                       ;; of `l`, and it may recur into the last element,
                       ;; but it should go only a couple of levels that way,
                       ;; since non-`group` tags have spanning locations
                       ;; gathered from their content
                       (let loop ([e l])
                         (cond
                           [(syntax? e) e]
                           [(not (pair? e)) #f]
                           [(null? (cdr e))
                            (define a (car e))
                            (if (and (pair? a)
                                     (syntax? (car a))
                                     (not (eq? (syntax-e (car a)) 'group)))
                                ;; found a tag like `block` or `alts`
                                (if (eq? (syntax-e (car a)) 'alts)
                                    (loop (let last ([a a])
                                            (if (null? (cdr a))
                                                (car a)
                                                (last (cdr a)))))
                                    (car a))
                                (loop a))]
                           [else (loop (cdr e))]))))
  (define s-loc (cond
                  [(not start-t)
                   (token-srcloc alt-start-t)]
                  [(syntax? start-t)
                   (syntax-srcloc start-t)]
                  [else
                   (token-srcloc start-t)]))
  (define e-loc (and last-t/e
                     (if (syntax? last-t/e)
                         (syntax-srcloc last-t/e)
                         (token-srcloc last-t/e))))
  (add-srcloc l
              s-loc
              (srcloc-spanning s-loc e-loc)))

(define (srcloc-spanning s-loc e-loc)
  (cond
    [e-loc
     (define s-position (srcloc-position s-loc))
     (srcloc (srcloc-source s-loc)
             (srcloc-line s-loc)
             (srcloc-column s-loc)
             s-position
             (if e-loc
                 (let ([s s-position]
                       [e (srcloc-position e-loc)]
                       [sp (srcloc-span e-loc)])
                   (and s e sp
                     (+ (max (- e s) 0) sp)))
                 (srcloc-span s-loc)))]
    [else s-loc]))

(define (token-raw t)
  (define value (token-value t))
  (or (syntax-raw-property value)
      (syntax-e value)))

(define (record-raw stx t pre-raw suffix-raw)
  (define e (syntax-e stx))
  (cond
    [(and (pair? e)
          (tag? 'op (car e)))
     (define new-op (record-raw (cadr e) t pre-raw suffix-raw))
     (define new-e (list (car e) new-op))
     (define new-stx (datum->syntax stx new-e stx stx))
     (if (syntax-raw-property new-stx)
         new-stx
         (syntax-raw-property new-stx '()))]
    [else
     (define stx+raw (if t
                         (syntax-raw-property stx (raw-cons (token-raw t)
                                                            (or (syntax-raw-property stx) '())))
                         (if (syntax-raw-property stx)
                             stx
                             (syntax-raw-property stx '()))))
     (define stx+pre-raw
       (if (null? pre-raw)
           stx+raw
           (add-raw-token-prefix stx+raw pre-raw (syntax-raw-prefix-property stx))))
     (if (null? suffix-raw)
         stx+pre-raw
         (syntax-raw-suffix-property stx+pre-raw (raw-cons (or (syntax-raw-suffix-property stx) '())
                                                           (raw-tokens->raw (reverse suffix-raw)))))]))

(define (add-raw-tail top raw)
  (if (null? raw)
      top
      (datum->syntax* top
                      (cons (syntax-raw-suffix-property (car (syntax-e top)) (raw-tokens->raw raw))
                            (cdr (syntax-e top)))
                      top
                      top)))

(define (add-pre-raw stx pre-raw)
  (if (null? pre-raw)
      stx
      (add-raw-token-prefix stx pre-raw '())))

(define (maybe-raw p) (and (not (null? p)) p))

;; if `pre-raw` contains a 'at token, then it's the boundary between a
;; normal prefix and inner prefix
(define (add-raw-token-prefix stx pre-raw post-prefix)
  (define-values (new-prefix inner-prefix) (raw-tokens->raw/prefix pre-raw))
  (cond
    [(null? inner-prefix)
     (syntax-raw-prefix-property stx (maybe-raw (raw-cons new-prefix post-prefix)))]
    [else
     (let ([stx (syntax-raw-prefix-property stx (maybe-raw new-prefix))])
       (syntax-raw-inner-prefix-property stx (maybe-raw (raw-cons inner-prefix post-prefix))))]))

(define (move-pre-raw from-stx to)
  (cond
    [(not (syntax? from-stx)) to]
    [else
     (define pre-raw (syntax-raw-prefix-property from-stx))
     (define inner-pre-raw (syntax-raw-inner-prefix-property from-stx))
     (cond
       [(or pre-raw inner-pre-raw)
        (define a (car to))
        (cons (let* ([a (if inner-pre-raw
                            (syntax-raw-inner-prefix-property a (raw-cons (raw-cons
                                                                           inner-pre-raw
                                                                           (syntax-raw-prefix-property a))
                                                                          (syntax-raw-inner-prefix-property a)))
                            a)]
                     [a (syntax-raw-prefix-property a (maybe-raw
                                                       (raw-cons pre-raw
                                                                 (and (not inner-pre-raw)
                                                                      (syntax-raw-prefix-property a)))))])
                a)
              (cdr to))]
       [else to])]))

(define (move-pre-raw* from-stx to)
  (cond
    [(syntax? (car to)) (move-pre-raw from-stx to)]
    [else (cons (move-pre-raw* from-stx (car to))
                (cdr to))]))

(define (move-post-raw-to-prefix from-stx to)
  (define post-raw (and (syntax? from-stx)
                        (raw-cons (or (syntax-raw-tail-property from-stx) null)
                                  (or (syntax-raw-suffix-property from-stx) null))))
  (cond
    [(and post-raw (not (null? post-raw)))
     (define a (datum->syntax* #f (car to)))
     (cons (syntax-raw-prefix-property a (raw-cons post-raw
                                                   (or (syntax-raw-prefix-property a) '())))
           (cdr to))]
    [else to]))

(define (move-post-raw-to-prefix* from-stx to)
  (cond
    [(syntax? (car to)) (move-post-raw-to-prefix from-stx to)]
    [else (cons (move-post-raw-to-prefix* from-stx (car to))
                (cdr to))]))

(define (raw-tokens->raw pre-raw)
  (for/list ([raw-t (in-list (reverse pre-raw))])
    (if (token? raw-t)
        (token-raw raw-t)
        raw-t)))

;; splits on an 'at token
(define (raw-tokens->raw/prefix pre-raw)
  (let loop ([pre-raw pre-raw] [accum null])
    (cond
      [(null? pre-raw) (values accum null)]
      [else
       (define raw-t (car pre-raw))
       (cond
         [(and (token? raw-t)
               (eq? 'at (token-name raw-t)))
          (values (raw-tokens->raw (cdr pre-raw))
                  (cons (token-raw raw-t) accum))]
         [else
          (loop (cdr pre-raw)
                (cons (if (token? raw-t)
                          (token-raw raw-t)
                          raw-t)
                      accum))])])))

(define (add-raw-to-prefix t pre-raw l #:tail [post-raw #f] #:tail-suffix [tail-suffix-raw null])
  (cons (let* ([stx (record-raw (car l) t pre-raw null)]
               [stx (if (and post-raw (not (null? post-raw)))
                        (syntax-raw-tail-property stx (raw-cons (or (syntax-raw-tail-property stx) '())
                                                                (raw-tokens->raw post-raw)))
                        stx)]
               [stx (if (null? tail-suffix-raw)
                        stx
                        (syntax-raw-suffix-property stx (raw-tokens->raw tail-suffix-raw)))])
          stx)
        (cdr l)))

(define (add-raw-to-prefix* t pre-raw l)
  (cond
    [(syntax? (car l)) (add-raw-to-prefix t pre-raw l)]
    [else (cons (add-raw-to-prefix* t pre-raw (car l))
                (cdr l))]))

(define (add-tail-raw-to-prefix post-raw post-stxes l)
  (cons (syntax-raw-tail-property (datum->syntax* #f (car l))
                                  (raw-cons (map syntax-to-raw post-stxes)
                                            (raw-tokens->raw post-raw)))
        (cdr l)))

(define (raw-cons a b)
  (combine-shrubbery-raw a b))

(define (syntax-property-copy dest src prop)
  (define v (prop src))
  (if v
      (prop dest v)
      dest))

;; like `datum->syntax`, but ensures that a sequence of pairs is not
;; too long before there's a syntax pair
(define (datum->syntax* ctx v [src #f] [props #f])
  (datum->syntax
   ctx
   (let loop ([v v] [depth 0])
     (cond
       [(pair? v)
        (cond
          [(depth . >= . 32)
           (datum->syntax ctx
                          (cons (loop (car v) 0) (loop (cdr v) 0))
                          src)]
          [else
           (cons (loop (car v) 0) (loop (cdr v) (add1 depth)))])]
       [else v]))
   src
   props))

;; raw-prefix and raw-suffix information to move it to an
;; enclosing group and to prefer suffix associations over
;; prefix associations
(define (normalize-group-raw s)
  (define (at-property elem update)
    (define e (syntax-e elem))
    (cond
      [(not (pair? e)) (if update
                           (update elem #f)
                           (values elem #f))]
      [else
       (case (syntax-e (car e))
         [(group parens brackets braces quotes block alts)
          (if update
              (datum->syntax #f (cons (update (car e) #t)
                                      (cdr e)))
              (values (car e) #t))]
         [(op)
          (if update
              (datum->syntax #f (list (car e)
                                      (update (cadr e) #f)))
              (values(cadr e) #f))]
         [else
          (if update
              (update elem #f)
              (values elem #f))])]))
  (define elem-raw-suffix
    (case-lambda
      [(elem)
       (define-values (s openclose?) (at-property elem #f))
       (syntax-raw-suffix-property s)]
      [(elem v)
       (at-property elem
                    (lambda (s openclose?)
                      (syntax-raw-suffix-property s v)))]))
  (define elem-raw-prefix
    (case-lambda
      [(elem)
       (define-values (s openclose?) (at-property elem #f))
       (syntax-raw-prefix-property s)]
      [(elem v)
       (at-property elem
                    (lambda (s openclose?)
                      (syntax-raw-prefix-property s v)))]))
  (define (shift-prefix-to-suffix elems-in
                                  #:extract-suffix? [extract-suffix? #f]
                                  #:add-suffix [add-suffix #f])
    ;; move element prefixes to preceding element suffixes,
    ;; and extract trailing suffix
    (define elems (for/list ([elem (in-list elems-in)])
                    (normalize-group-raw elem)))
    (cond
      [(null? elems) (values null add-suffix)]
      [else
       (let loop ([elems elems])
         (define elem (car elems))
         (cond
           [(null? (cdr elems))
            (cond
              [(not extract-suffix?)
               (values (if add-suffix
                           (list (elem-raw-suffix elem (raw-cons
                                                        (elem-raw-suffix elem)
                                                        add-suffix)))
                           elems)
                       #f)]
              [(elem-raw-suffix elem)
               => (lambda (suffix)
                    (values (list (elem-raw-suffix elem #f))
                            suffix))]
              [else (values elems #f)])]
           [(elem-raw-prefix (cadr elems))
            => (lambda (prefix)
                 (let ([elem (elem-raw-suffix elem
                                              (raw-cons
                                               (or (elem-raw-suffix elem) null)
                                               prefix))])
                   (define-values (new-elems suffix)
                     (loop (cons (elem-raw-prefix (cadr elems) #f)
                                 (cddr elems))))
                   (values (cons elem new-elems)
                           suffix)))]
           [else
            (define-values (new-elems suffix) (loop (cdr elems)))
            (values (cons elem new-elems)
                    suffix)]))]))
  (cond
    [(not (pair? (syntax-e s))) s]
    [else
     (define head (car (syntax-e s)))
     (case (syntax-e head)
       [(multi)
        (define-values (gs suffix) (shift-prefix-to-suffix (cdr (syntax->list s))
                                                           #:add-suffix (syntax-raw-suffix-property head)))
        (datum->syntax #f (cons (syntax-raw-suffix-property head suffix)
                                gs))]
       [(group)
        (define-values (elems suffix) (shift-prefix-to-suffix (cdr (syntax->list s))
                                                              #:extract-suffix? #t))
        (let* ([head (if suffix
                         (syntax-raw-suffix-property head
                                                     (raw-cons
                                                      suffix
                                                      (or (syntax-raw-suffix-property head) null)))
                         head)]
               ;; move initial element prefix to group prefix
               [pre (and (pair? elems) (elem-raw-prefix (car elems)))]
               [head (if pre
                         (syntax-raw-prefix-property head
                                                     (raw-cons
                                                      (or (syntax-raw-prefix-property head) null)
                                                      pre))
                         head)]
               [elems (if pre
                          (cons (elem-raw-prefix (car elems) #f)
                                (cdr elems))
                          elems)])
          (datum->syntax #f (cons head elems)))]
       [(parens brackets braces quotes block alts)
        ;; normalize inside, but cannot move leading prefix or trailing suffix
        ;; to overall prefix or (tail) suffix --- except in the case of `block` or `alts`,
        ;; but we choose to leave the suffix inside in those cases, too
        (define-values (gs no-suffix) (shift-prefix-to-suffix (cdr (syntax->list s))))
        (datum->syntax #f (cons head gs))]
       [else s])]))

(define (syntax-to-raw s)
  (define-values (pfx raw sfx)
    (shrubbery-syntax->raw s #:keep-suffix? #t))
  (raw-cons raw sfx))

;; ----------------------------------------

(define (parse-all in
                   #:source [source (object-name in)]
                   #:mode [mode 'top] ; 'top, 'text, 'interactive, or 'line
                   #:start-column [start-column 0])
  (define l (lex-all in fail
                     #:source source
                     #:mode mode
                     #:consume-eof? #t
                     #:start-column start-column))
  (define v (if (eq? mode 'text)
                (parse-text-sequence l 0 zero-delta (lambda (c l line delta) (datum->syntax #f c)))
                (parse-top-groups l #:interactive? (memq mode '(interactive line)))))
  (if (syntax? v)
      (normalize-group-raw v)
      v))

(module+ main
  (require racket/cmdline
           "print.rkt")

  (define show-raw? #f)
  (define show-property? #f)
  (define text-mode? #f)
  (define one-line? #f)

  (define (parse-all* in)
    (unless one-line?
      (port-count-lines! in))
    (define e (parse-all in #:mode (if text-mode? 'text 'top)))
    (unless (eof-object? e)
      (cond
        [(or show-raw? show-property?)
         (for ([s (in-list (syntax->list e))])
           (when show-raw?
             (printf "#|\n~a\n|#\n" (shrubbery-syntax->string s
                                                              #:keep-prefix? #t
                                                              #:keep-suffix? #t)))
           (when show-property?
             (show-properties s))
           (pretty-write (syntax->datum s)))]
        [else
         (pretty-write
          (syntax->datum e))])))

  (define (show-properties s #:head? [head? #t] #:skip-suffix? [skip-suffix? #f])
    (cond
      [(pair? s)
       (define a (car s))
       (show-properties a #:skip-suffix? head?)
       (show-properties (cdr s) #:head? #f)
       (define tail (syntax-raw-tail-property a))
       (when tail
         (printf " ~s... ~s\n" (syntax->datum a) tail))
       (define tail-suffix (and head? (syntax-raw-suffix-property a)))
       (when tail-suffix
         (printf " ~s.... ~s\n" (syntax->datum a) tail-suffix))]
      [(null? s) (void)]
      [else
       (define (raw-consx a b) (and (or a b) (raw-cons a b)))
       (define prefix (raw-consx (syntax-raw-prefix-property s)
                                 (syntax-raw-inner-prefix-property s)))
       (define raw (syntax-raw-property s))
       (define suffix (and (not skip-suffix?)
                           (raw-consx (syntax-raw-suffix-property s)
                                      (syntax-raw-inner-suffix-property s))))
       (printf "~.s: ~a~s~a\n"
               (syntax->datum s)
               (if prefix
                   (format "~s+ " prefix)
                   "")
               raw
               (if suffix
                   (format " +~s" suffix)
                   ""))
       (define e (syntax-e s))
       (when (pair? e)
         (show-properties e))]))

  (command-line
   #:once-each
   [("--text") "Start as if within `@{`...`}`"
               (set! text-mode? #t)]
   [("--recover") "Continue parsing after an error"
                  (current-recover-mode #t)]
   [("--raw") "Show raw strings when printing"
              (set! show-raw? #t)]
   [("--property") "Show raw-string properties when printing"
                   (set! show-property? #t)]
   [("--one-line") "Disable line counting to assume a single line"
                   (set! one-line? #t)]
   #:args file
   (if (null? file)
       (parse-all* (current-input-port))
       (for-each (lambda (file)
                   (call-with-input-file*
                    file
                    parse-all*))
                 file))))
