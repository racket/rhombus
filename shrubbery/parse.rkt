#lang racket/base
(require racket/pretty
         "lex.rkt"
         "srcloc.rkt"
         (submod "print.rkt" for-parse)
         "private/property.rkt"
         "private/at-space.rkt")

(provide parse-all)

;; Parsing state at the group level:
(struct state (count?          ; parsed based on lines and columns?
               line            ; current group's line and last consumed token's line
               column          ; group's column; below ends group, above starts indented
               operator-column ; column for operator that continues group on a new line
               paren-immed?    ; immediately in `()` or `[]`?
               bar-closes?     ; does `|` always end a group?
               bar-closes-line ; `|` (also) ends a group on this line
               block-mode      ; 'inside, #f, `:` or `|` token, or 'end
               can-empty?      ; in a context where a `:` can be empty?
               delta           ; a `cont-delta`, tracks `\` continuations
               raw             ; reversed whitespace (and comments) to be remembered
               at-mode))       ; look for `@` continuation after term: #f, 'initial, or 'no-initial

(define (make-state #:count? count?
                    #:line line
                    #:column column
                    #:operator-column [operator-column #f]
                    #:paren-immed? [paren-immed? #f]
                    #:bar-closes? [bar-closes? #f]
                    #:bar-closes-line [bar-closes-line #f]
                    #:block-mode [block-mode #f]
                    #:can-empty? [can-empty? #t]
                    #:delta delta
                    #:raw [raw null])
  (state count?
         line
         column
         operator-column
         paren-immed?
         bar-closes?
         bar-closes-line
         block-mode
         can-empty?
         delta
         raw
         #f))

;; Parsing state for group sequences: top level, in opener-closer, or after `:`
(struct group-state (count?         ; parsed based on lines and columns?
                     closer         ; expected closer: a string, EOF, or column (maybe 'any as column)
                     paren-immed?   ; immediately in `()` or `[]`?
                     column         ; not #f => required indentation check checking
                     check-column?  ; #f => allow any sufficiently large (based on closer) indentation
                     bar-closes?    ; does `|` always end the sequence of groups?
                     bar-closes-line ; `|` (also) ends a sequence of groups on this line
                     block-mode     ; 'inside, #f, `:` or `|` token, or 'end
                     can-empty?      ; in a context where a `:` can be empty?
                     comma-time?    ; allow and expect a comma next
                     sequence-mode  ; 'any, 'one, or 'none
                     last-line      ; most recently consumed line
                     delta          ; a `cont-delta`, tracks `\` continuations
                     commenting     ; pending group-level `#//` token; exclusive with `tail-commenting`
                     tail-commenting ; pending group-level `#//` at end (so far)
                     raw))          ; reversed whitespace (and comments) to be remembered

(define (make-group-state #:count? count?
                          #:closer closer
                          #:paren-immed? [paren-immed? #f]
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
               paren-immed?
               column
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

(define (closer-column? c) (or (eq? c 'any) (number? c)))

(define closer-expected? pair?)
(define (closer-expected closer) (if (pair? closer) (car closer) closer))
(define (closer-expected-opener closer) (and (pair? closer) (cdr closer)))
(define (make-closer-expected str tok) (cons str tok))

(define group-tag (syntax-raw-property (datum->syntax #f 'group) '()))
(define top-tag (syntax-raw-property (datum->syntax #f 'top) '()))
(define parens-tag (syntax-raw-property (datum->syntax #f 'parens) '()))
(define brackets-tag (syntax-raw-property (datum->syntax #f 'brackets) '()))

(define within-parens-str "within parentheses, brackets, or braces")

;; ----------------------------------------

;; In the parsed representation of a shrubbery, source locations are
;; not associated with sequences tagged `group` or `top` (but they may
;; have 'raw-prefix and/or 'raw-tail, as described below). For terms
;; tagged with `parens`, `braces`, `block`, `alts`, and `op`, the same
;; source location is associated with the tag and the parentheses that
;; group the tag with its members, and that location spans the content
;; and any delimiters used to form it (such as parentheses or a `:`
;; that starts a block).

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
      (unless (eqv? column (group-state-column sg))
        (fail t "wrong indentation"))))
  (define closer (group-state-closer sg))
  (cond
    [(null? l)
     ;; Out of tokens
     (when (string? (closer-expected closer))
       (fail (closer-expected-opener closer) (format "expected ~s" (closer-expected closer))))
     (done)]
    [else
     (define t (car l))
     (define column (column+ (token-column t) (cont-delta-column (group-state-delta sg))))
     (define (less-indented?)
       (and (group-state-count? sg)
            (closer-column? closer)
            column
            (column . < . closer)))
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
                                           [column column]))])])]
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
                (fail t (format "expected ~s; closing at ~a" (closer-expected closer) (token-value t))))
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
              (unless (and (group-state-paren-immed? sg)
                           (group-state-comma-time? sg))
                (fail t (format "misplaced comma~a"
                                (if (group-state-paren-immed? sg)
                                    ""
                                    (string-append " (not immediately " within-parens-str ")")))))
              (define-values (rest-l last-line delta raw)
                (next-of (cdr l) (token-line t) (group-state-delta sg) (cons t (group-state-raw sg))
                         (group-state-count? sg)))
              ;; In top level or immediately in opener-closer: 
              (parse-groups rest-l (struct-copy group-state sg
                                                [check-column? (next-line?* rest-l last-line)]
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
                                                            #:paren-immed? #f
                                                            #:block-mode #f
                                                            #:can-empty? #f
                                                            #:column #f
                                                            #:last-line splice-last-line
                                                            #:delta splice-delta
                                                            #:commenting group-commenting
                                                            #:raw splice-raw)))
                 (when (group-state-paren-immed? sg)
                   (unless (= 1 (length gs))
                     (fail t (format "multi-group splice not allowed (immediately ~a)" within-parens-str))))
                 (define-values (more-gs more-l more-line more-delta more-end-t more-tail-commenting more-tail-raw)
                   (parse-groups close-l (struct-copy group-state sg
                                                      [comma-time? (group-state-paren-immed? sg)]
                                                      [check-column? (next-line?* close-l close-line)]
                                                      [column (or (group-state-column sg) column)]
                                                      [last-line close-line]
                                                      [delta close-delta]
                                                      [commenting #f]
                                                      [tail-commenting #f]
                                                      [block-mode (next-block-mode (group-state-block-mode sg))]
                                                      [can-empty? #f]
                                                      [raw group-tail-raw])))
                 (values (append gs more-gs)
                         more-l more-line more-delta more-end-t more-tail-commenting more-tail-raw)]
                
                [else
                 (when (group-state-paren-immed? sg)
                   (fail t (format "misplaced semicolon (immediately ~a)" within-parens-str)))
                 (parse-groups rest-l (struct-copy group-state sg
                                                   [check-column? (next-line?* rest-l last-line)]
                                                   [column (or (group-state-column sg) column)]
                                                   [last-line last-line]
                                                   [delta delta]
                                                   [commenting (group-state-tail-commenting sg)]
                                                   [tail-commenting #f]
                                                   [block-mode (next-block-mode (group-state-block-mode sg))]
                                                   [raw raw]))])])]
          [else
           (when (group-state-comma-time? sg)
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
                [(eq? (group-state-block-mode sg) 'inside)
                 ;; Bar at the start of a group
                 (define same-line? (or (not (group-state-count? sg))
                                        (not (group-state-last-line sg))
                                        (= line (group-state-last-line sg))))
                 (when (group-state-check-column? sg)
                   (unless (or same-line?
                               (= column (group-state-column sg)))
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
                                                     [check-column? (next-line?* rest-l group-end-line)]
                                                     [last-line group-end-line]
                                                     [delta group-end-delta]
                                                     [comma-time? (group-state-paren-immed? sg)]
                                                     [commenting #f]
                                                     [tail-commenting block-tail-commenting]
                                                     [raw (if commenting
                                                              (append block-tail-raw
                                                                      (cons (syntax-to-raw (datum->syntax #f g))
                                                                            pre-raw))
                                                              block-tail-raw)])))
                 (values (if commenting
                             gs
                             (cons (list group-tag
                                         (add-span-srcloc
                                          t end-t
                                          (cons 'bar g)))
                                   gs))
                         rest-rest-l
                         end-line
                         end-delta
                         end-t
                         tail-commenting
                         tail-raw)]
                [else
                 (if (and (token? (group-state-block-mode sg))
                          (eq? 'block-operator (token-name (group-state-block-mode sg))))
                     (fail (group-state-block-mode sg) "unnecessary `:` before `|`")
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
                (fail t "second group not allowed within `@«` and `»`"))
              (define-values (g rest-l group-end-line group-delta group-tail-commenting group-tail-raw)
                (parse-group l (make-state #:count? (group-state-count? sg)
                                           #:paren-immed? (group-state-paren-immed? sg)
                                           #:line line
                                           #:column use-column
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
                                                  [comma-time? (group-state-paren-immed? sg)]
                                                  [delta group-delta]
                                                  [commenting #f]
                                                  [tail-commenting group-tail-commenting]
                                                  [raw (if commenting
                                                           (append group-tail-raw
                                                                   (cons (syntax-to-raw (datum->syntax #f g))
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
     (define (check-nested-block-mode t)
       (when (eq? (state-block-mode s) 'no)
         (fail t "blocks not allowed immediately within `@«` and `»`")))
     ;; Consume a token
     (define (keep delta
                   #:operator-column [operator-column (state-operator-column s)]
                   #:at-mode [at-mode (state-at-mode s)]
                   #:suffix? [suffix? #t])
       (check-block-mode)
       (define-values (suffix-raw suffix-l suffix-line suffix-delta)
         (if suffix?
             (get-suffix-comments (cdr l) line delta)
             (values null (cdr l) line delta)))
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
          (check-block-mode)
          (check-nested-block-mode t)
          (parse-block #f l
                       #:count? (state-count? s)
                       #:block-mode 'inside
                       #:line line
                       #:closer (or (and (state-count? s) (token-column t)) 'any)
                       #:bar-closes? #f
                       #:bar-closes-line #f
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
             [(column . > . (state-column s))
              ;; More indented forms a nested block when there's
              ;; a preceding `:` (doesn't get here) or starting with `|`;
              ;; more indented continues a group when starting with an
              ;; operator
              (cond
                [(eq? 'bar-operator (token-name use-t))
                 (when (and (state-operator-column s)
                            (<= column (state-operator-column s)))
                   (fail use-t "wrong indentation"))
                 (check-nested-block-mode use-t)
                 (parse-block #f use-l
                              #:count? (state-count? s)
                              #:block-mode 'inside
                              #:line (token-line use-t)
                              #:closer (or column 'any)
                              #:bar-closes? #f
                              #:bar-closes-line #f
                              #:delta delta
                              #:raw raw
                              #:group-commenting group-commenting)]
                [(and (eq? 'operator (token-name use-t))
                      (or (not (state-operator-column s))
                          (= column (state-operator-column s))))
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
           (check-nested-block-mode t)
           (parse-block t (cdr l)
                        #:count? (state-count? s)
                        #:line line
                        #:closer (or (and (state-count? s)
                                          (column-half-next (or (state-operator-column s)
                                                                (state-column s))))
                                     'any)
                        #:delta (state-delta s)
                        #:raw (state-raw s)
                        #:bar-closes? (and (state-bar-closes? s)
                                           (not (state-bar-closes-line s)))
                        #:bar-closes-line (state-bar-closes-line s)
                        #:can-empty? (state-can-empty? s)
                        #:could-empty-if-start? #t)]
          [(bar-operator)
           (parse-alts-block t l)]
          [(opener)
           (check-block-mode)
           (define-values (closer tag paren-immed?)
             (case (token-e t)
               [("(") (values ")" 'parens #t)]
               [("{") (values "}" 'braces #t)]
               [("[") (values "]" 'brackets #t)]
               [("'") (values "'" 'quotes #f)]
               [("«") (if (state-at-mode s)
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
                                                    #:paren-immed? paren-immed?
                                                    #:block-mode (if (eq? tag 'at) 'no 'start)
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
               [else (values rest-l0 close-line0 close-delta0 end-t0 group-tail-raw0)]))
           (define-values (suffix-raw suffix-l suffix-line suffix-delta)
             (get-suffix-comments rest-l close-line close-delta))
           (define-values (at-adjust new-at-mode at-l at-line at-delta)
             (continue-at (state-at-mode s) (equal? closer "]") suffix-l suffix-line suffix-delta (state-count? s)))
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
           (cond
             [(null? (cdr l))
              (fail t "missing term after `@`")
              (parse-group null s)]
             [else
              (define next-t (cadr l))
              (check-same-line t next-t (state-count? s))
              (case (token-name next-t)
                [(opener)
                 (case (token-e next-t)
                   [("(" "«" "'")
                    (parse-group (cdr l) (struct-copy state s
                                                      [raw (cons t (state-raw s))]
                                                      [at-mode 'initial]))]
                   [("[")
                    (keep (state-delta s) #:at-mode 'no-initial #:suffix? #f)]
                   [else (error "unexpected" (token-name next-t)  (token-e next-t))])]
                [(identifier number literal operator opener)
                 (parse-group (cdr l) (struct-copy state s
                                                   [raw (cons t (state-raw s))]
                                                   [at-mode 'initial]))]
                [(at-opener)
                 (keep (state-delta s) #:at-mode 'no-initial #:suffix? #f)]
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
                     #:delta in-delta
                     #:raw in-raw
                     #:group-commenting [in-group-commenting #f]
                     #:block-mode [block-mode t]
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
     (define-values (null-g post-l post-line post-delta post-tail-commenting post-tail-raw)
       (if used-closer?
           ;; in 'end mode, so errors or returns a null group:
           (parse-group rest-l (make-state #:count? count?
                                           #:line end-line
                                           #:column +inf.0
                                           #:bar-closes? bar-closes?
                                           #:bar-closes-line bar-closes-line
                                           #:block-mode 'end
                                           #:delta end-delta
                                           #:raw null))
           (values '() rest-l end-line end-delta tail-commenting (if used-closer?
                                                                     null
                                                                     tail-raw))))
     (unless (null? null-g) (error "internal error, parsed more"))
     (values (list (add-raw-to-prefix
                    t in-raw #:tail (let ([tail-raw (and used-closer? tail-raw)])
                                      (if (pair? rev-suffix-raw)
                                          (append (or tail-raw null) rev-suffix-raw)
                                          tail-raw))
                    (add-span-srcloc
                     t end-t #:alt next-t
                     (tag-as-block indent-gs))))
             post-l
             post-line
             post-delta
             post-tail-commenting
             post-tail-raw)]
    [else
     (when opener-t (fail opener-t (format "expected `»`")))
     (when (and (not can-empty?) t (not opener-t)) (fail-empty))
     (values (list (add-raw-to-prefix
                    t in-raw
                    (add-span-srcloc
                     t #f
                     (tag-as-block null))))
             next-l
             line
             delta
             group-commenting
             raw)]))

(define (tag-as-block gs)
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
     (cons 'alts (for/list ([g (in-list gs)])
                   (move-pre-raw
                    (car g)
                    (let ([b (cadr g)])
                      (cadr b)))))]
    [else (cons 'block gs)]))

(define (bars-insert-alts gs)
  (cond
    [(and (pair? gs)
          ;; same check is in `tag-as-block
          (let ([g (car gs)])
            (and (pair? g)
                 (tag? 'group (car g))
                 (pair? (cdr g))
                 (null? (cddr g))
                 (let ([b (cadr g)])
                   (and (pair? b)
                        (tag? 'bar (car b))
                        (car b))))))
     => (lambda (bar-tag)
          (list (list group-tag
                      (add-span-srcloc
                       bar-tag #f
                       (tag-as-block gs)))))]
    [else gs]))

(define (tag? sym e)
  (or (eq? sym e)
      (and (syntax? e)
           (eq? sym (syntax-e e)))))

;; Look for `{` (as 'at-opener) next or a `[` that might be followed
;; by a `{`, and prepare to convert by rearranging info a splice
;; followed by parentheses
(define (continue-at at-mode after-bracket? l line delta count?)
  (define (at-call rator parens g)
    (if (eq? at-mode 'no-initial)
        (cons (move-pre-raw rator
                            (add-raw-to-prefix #f (syntax-to-raw rator) parens))
              g)
        (list* rator parens g)))
  (cond
    [(not at-mode)
     (values (lambda (g) g) #f l line delta)]
    [(and (not after-bracket?)
          (pair? l)
          (eq? 'opener (token-name (car l)))
          (equal? "[" (token-e (car l))))
     (values (lambda (g)
               (define a (cadr g))
               (define tag (car a))
               (cond
                 [(tag? 'brackets tag)
                  (at-call (car g)
                           (cons (datum->syntax tag 'parens tag tag)
                                 (cdr a))
                           (cddr g))]
                 [(eq? at-mode 'no-initial)
                  (add-raw-to-prefix* #f (syntax-to-raw (car g))
                                      (cdr g))]
                 [else g]))
             at-mode l line delta)]
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
             (values (lambda (g) (cond
                                   [(not after-bracket?)
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
                                                  (add-raw-to-prefix* #f (syntax-to-raw bracket)
                                                                      new-g))]))
                     'initial (if (null? l) null (cdr l)) line delta)]))))]
    [else
     (values (lambda (g) g) #f l line delta)]))

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
                         (datum->syntax (token-value opener-t)
                                        'brackets
                                        (token-value opener-t)
                                        (token-value opener-t))
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
       ;; `parse-group` work will be delimited by 'at-content or 'at-closer
       (define-values (g rest-l group-end-line group-delta group-tail-commenting group-tail-raw)
         (parse-group (if comment?
                          (cons (token-rename t 'at) (cdr l))
                          l)
                      (make-state #:count? count?
                                  #:line (token-line t)
                                  #:column (token-column t)
                                  #:delta zero-delta
                                  #:raw null)))
       (loop rest-l
             (cons (if comment?
                       (list 'comment (cons (token-raw t) (syntax-to-raw g)))
                       (cons group-tag g))
                   content))]
      [(comment)
       (loop (cdr l) (cons (list 'comment (token-e (car l))) content))]
      [else (error "unexpected in at" (token-name (car l)))])))

(define (splice-at t g tail-raw)
  (define gs (car g))
  (define at (car gs))
  (unless (tag? 'at at) (error "expected at"))
  (when (null? (cdr gs)) (fail t "empty group within `@«` and `»`"))
  (unless (null? (cddr gs)) (error "extra groups in at"))
  (define rest (cdr g))
  (values
   (move-pre-raw* at
                  (add-raw-to-prefix* #f (syntax-to-raw at)
                                      (append (cdadr gs)
                                              (if (null? rest)
                                                  rest
                                                  (move-post-raw-to-prefix at rest)))))
   (if (null? rest)
       (append tail-raw (list (or (syntax-raw-tail-property at) '())))
       tail-raw)))

;; Like `datum->syntax`, but propagates the source location of
;; a start of a list (if any) to the list itself. That starting
;; item is expected to be a tag that has the span of the whole
;; term already as its location
(define (lists->syntax l)
  (cond
    [(pair? l)
     (define a (car l))
     (define new-l (for/list ([e (in-list l)])
                     (lists->syntax e)))
     (if (syntax? a)
         (datum->syntax* #f new-l a stx-for-original-property)
         (datum->syntax* #f new-l))]
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
    [(null? l) (values null (or last-line 0) delta raw)]
    [else
     (define t (car l))
     (case (token-name t)
       [(whitespace comment)
        (next-of (cdr l) last-line delta (cons (car l) raw) count?)]
       [(continue-operator)
        (define line (token-line t))
        ;; a continue operator not followed only by whitespace and
        ;; comments is just treated as whitespace
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
           ;; like whitespace:
           (next-of next-l last-line delta next-raw count?)]
          [else
           (define accum-delta? (or (not count?) (not last-line) (eqv? line last-line)))
           (next-of next-l
                    ;; whitespace-only lines don't count, so next continues
                    ;; on the same line by definition:
                    #f
                    (cont-delta (column+ (token-column t)
                                         (+ (if accum-delta?
                                                (cont-delta-column delta)
                                                0)
                                            1))
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
          (case (token-name t)
            [(whitespace)
             (loop (cdr raw) (cons t pending) rev-suffix-raw)]
            [(comment)
             (cond
               [(eqv? column (token-column t))
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
       (if (integer? c)
           (add1 c)
           (add1 (inexact->exact (floor c))))))

(define (column-half-next c)
  (if (integer? c)
      (+ c 0.5)
      (column-next c)))

(define (column+ c n)
  (and c (+ c n)))

(define (line+ l n)
  (and l (+ l n)))

(define (next-block-mode mode)
  (if (eq? mode 'no) 'no #f))

;; ----------------------------------------

(define (add-span-srcloc start-t end-t l #:alt [alt-start-t #f])
  (define (add-srcloc l loc)
    (cons (let ([stx (datum->syntax* #f (car l) loc stx-for-original-property)])
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
                       ;; gather from their content
                       (let loop ([e l])
                         (cond
                           [(syntax? e) e]
                           [(not (pair? e)) #f]
                           [(null? (cdr e))
                            (define a (car e))
                            (if (and (pair? a)
                                     (syntax? (car a)))
                                ;; found a tag like `block`
                                (car a)
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
  (define s-position (srcloc-position s-loc))
  (add-srcloc l (srcloc (srcloc-source s-loc)
                        (srcloc-line s-loc)
                        (srcloc-column s-loc)
                        s-position
                        (if e-loc
                            (let ([s s-position]
                                  [e (srcloc-position e-loc)]
                                  [sp (srcloc-span e-loc)])
                              (and s e sp
                                   (+ (max (- e s) 0) sp)))
                            (srcloc-span s-loc)))))

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
           (syntax-raw-prefix-property stx+raw (raw-cons (raw-tokens->raw pre-raw)
                                                         (or (syntax-raw-prefix-property stx) '())))))
     (if (null? suffix-raw)
         stx+pre-raw
         (syntax-raw-suffix-property stx+pre-raw (raw-cons (or (syntax-raw-suffix-property stx) '())
                                                           (raw-tokens->raw (reverse suffix-raw)))))]))

(define (add-raw-tail top raw)
  (if (null? raw)
      top
      (datum->syntax* top
                      (cons (syntax-raw-tail-property (car (syntax-e top)) (raw-tokens->raw raw))
                            (cdr (syntax-e top)))
                      top
                      top)))

(define (add-pre-raw stx pre-raw)
  (if (null? pre-raw)
      stx
      (syntax-raw-prefix-property stx (raw-tokens->raw pre-raw))))

(define (move-pre-raw from-stx to)
  (define pre-raw (and (syntax? from-stx)
                       (syntax-raw-prefix-property from-stx)))
  (cond
    [pre-raw
     (define a (car to))
     (cons (syntax-raw-prefix-property a (raw-cons pre-raw
                                                   (or (syntax-raw-prefix-property a) '())))
           (cdr to))]
    [else to]))

(define (move-pre-raw* from-stx to)
  (cond
    [(syntax? (car to)) (move-pre-raw from-stx to)]
    [else (cons (move-pre-raw* from-stx (car to))
                (cdr to))]))

(define (move-post-raw-to-prefix from-stx to)
  (define post-raw (and (syntax? from-stx)
                        (raw-cons (or (syntax-raw-tail-property from-stx) null)
                                  (or (syntax-raw-tail-suffix-property from-stx) null))))
  (cond
    [(and post-raw (not (null? post-raw)))
     (define a (datum->syntax* #f (car to)))
     (cons (syntax-raw-prefix-property a (raw-cons post-raw
                                                   (or (syntax-raw-prefix-property a) '())))
           (cdr to))]
    [else to]))

(define (raw-tokens->raw pre-raw)
  (for/list ([raw-t (in-list (reverse pre-raw))])
    (if (token? raw-t)
        (token-raw raw-t)
        raw-t)))

(define (add-raw-to-prefix t pre-raw l #:tail [post-raw #f] #:tail-suffix [tail-suffix-raw null])
  (cons (let* ([stx (record-raw (car l) t pre-raw null)]
               [stx (if (and post-raw (not (null? post-raw)))
                        (syntax-raw-tail-property stx (raw-cons (or (syntax-raw-tail-property stx) '())
                                                                (raw-tokens->raw post-raw)))
                        stx)]
               [stx (if (null? tail-suffix-raw)
                        stx
                        (syntax-raw-tail-suffix-property stx (raw-tokens->raw tail-suffix-raw)))])
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
  (cond
    [(null? a) b]
    [(null? b) a]
    [else (cons a b)]))

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

;; ----------------------------------------

;; check that line-counting is consistent (always on or always off),
;; and when it's off, make sure there are no newlines except maybe at
;; the beginning and/or end
(define (check-line-counting l)
  (unless (null? l)
    (define (fail-inconsistent t)
      (fail t "port did not consistently report lines and columns"))
    (let loop ([l l] [saw-non-ws? #f] [newline-t #f] [stack '()])
      (unless (null? l)
        (define t (car l))
        (unless (and (token-line t)
                     (token-column t))
          (when (or (token-line t)
                    (token-column t))
            (fail-inconsistent t))
          (when (and saw-non-ws?
                     (null? stack)
                     newline-t)
            (fail newline-t "port does not count lines, but input includes a newline outside of `«` and `»`")))
        (case (token-name t)
          [(whitespace comment continue-operator)
           (loop (cdr l)
                 saw-non-ws?
                 (and saw-non-ws?
                      (or newline-t
                          (and (null? stack)
                               (regexp-match? #rx"[\r\n]" (syntax-e (token-value t)))
                               t)))
                 stack)]
          [else
           (cond
             [(and (eq? 'opener (token-name t))
                   (equal? (token-e t) "«"))
              (loop (cdr l) #t newline-t (cons t stack))]
             [(and (eq? 'closer (token-name t))
                   (equal? (token-e t) "»"))
              (loop (cdr l) #f newline-t (if (pair? stack) (cdr stack) '()))]
             [else
              (loop (cdr l) #t newline-t stack)])])))))

;; ----------------------------------------

(define (parse-all in
                   #:source [source (object-name in)]
                   #:interactive? [interactive? #f]
                   #:text-mode? [text-mode? #f])
  (define l (lex-all in fail
                     #:source source
                     #:interactive? interactive?
                     #:text-mode? text-mode?
                     #:consume-eof? #t))
  (check-line-counting l)
  (define v (if text-mode?
                (parse-text-sequence l 0 zero-delta (lambda (c l line delta) (datum->syntax #f c)))
                (parse-top-groups l #:interactive? interactive?)))
  v)
  
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
    (define e (parse-all in #:text-mode? text-mode?))
    (unless (eof-object? e)
      (cond
        [(or show-raw? show-property?)
         (for ([s (syntax->list e)])
           (when show-raw?
             (printf "#|\n~a\n|#\n" (shrubbery-syntax->string s)))
           (when show-property?
             (show-properties s))
           (pretty-write (syntax->datum s)))]
        [else
         (pretty-write
          (syntax->datum e))])))

  (define (show-properties s)
    (cond
      [(pair? s)
       (define a (car s))
       (show-properties a)
       (show-properties (cdr s))
       (define tail (syntax-raw-tail-property a))
       (when tail
         (printf " ~s... ~s\n" a tail))]
      [(null? s) (void)]
      [else
       (define prefix (syntax-raw-prefix-property s))
       (define raw (syntax-raw-property s))
       (define suffix (syntax-raw-suffix-property s))
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
