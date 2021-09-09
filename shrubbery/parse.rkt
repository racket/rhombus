#lang racket/base
(require racket/pretty
         "lex.rkt"
         "srcloc.rkt"
         (submod "print.rkt" for-parse))

(provide parse-all)

;; Parsing state at the group level:
(struct state (line            ; current group's line and last consumed token's line
               column          ; group's column; below ends group, above starts indented
               paren-immed?    ; immediately in `()` or `[]`?
               bar-closes?     ; does `|` always end a group?
               bar-closes-line ; `|` (also) ends a group on this line
               block-mode      ; 'inside, #f, `:` token, or 'end
               delta           ; column delta created by `\`, applied to `line` continuation
               raw))           ; reversed whitespace (and comments) to be remembered

(define (make-state #:line line
                    #:column column
                    #:paren-immed? [paren-immed? #f]
                    #:bar-closes? [bar-closes? #f]
                    #:bar-closes-line [bar-closes-line #f]
                    #:block-mode [block-mode #f]
                    #:delta delta
                    #:raw [raw null])
  (state line
         column
         paren-immed?
         bar-closes?
         bar-closes-line
         block-mode
         delta
         raw))

;; Parsing state for group sequences: top level, in opener-closer, or after `:`
(struct group-state (closer         ; expected closer: a string, EOF, or column
                     paren-immed?   ; immediately in `()` or `[]`?
                     column         ; not #f => required indentation check checking
                     check-column?  ; #f => allow any sufficiently large (based on closer) indentation
                     bar-closes?    ; does `|` always end the sequence of groups?
                     bar-closes-line ; `|` (also) ends a sequence of groups on this line
                     block-mode     ; 'inside, #f, `:` token, or 'end
                     comma-time?    ; allow and expect a comma next
                     last-line      ; most recently consumed line
                     delta          ; column delta created by `\`, applies to `last-line` continuation
                     commenting     ; pending group-level `#//` token; exclusive with `tail-commenting`
                     tail-commenting ; pending group-level `#//` at end (so far)
                     raw))          ; reversed whitespace (and comments) to be remembered

(define (make-group-state #:closer closer
                          #:paren-immed? [paren-immed? #f]
                          #:column column
                          #:check-column? [check-column? #t]
                          #:bar-closes? [bar-closes? #f]
                          #:bar-closes-line [bar-closes-line #f]
                          #:block-mode [block-mode #f]
                          #:last-line last-line
                          #:delta delta
                          #:commenting [commenting #f]
                          #:raw [raw null])
  (group-state closer
               paren-immed?
               column
               check-column?
               bar-closes?
               bar-closes-line
               block-mode
               #f
               last-line
               delta
               #f
               commenting
               raw))

(define closer-column? number?)

(define closer-expected? pair?)
(define (closer-expected closer) (if (pair? closer) (car closer) closer))
(define (closer-expected-opener closer) (and (pair? closer) (cdr closer)))
(define (make-closer-expected str tok) (cons str tok))

(define group-tag (syntax-property (datum->syntax #f 'group) 'raw ""))

;; ----------------------------------------

;; In the parsed representation of a shrubbery, source locations are
;; not associated with sequences tagged `group` or `top`. For terms
;; tagged with `parens`, `braces`, `block`, `alts`, and `op`, the same
;; source location is associated with the tag and the parentheses that
;; group the tag with its members, and that location spans the content
;; and any delimiters used to form it (such as parentheses or a `:`
;; that starts a block).

;; Parse all groups in a stream
(define (parse-top-groups l)
  (define-values (gs rest-l end-line end-delta end-t tail-commenting tail-raw)
    (parse-groups l (make-group-state #:closer eof
                                      #:column #f
                                      #:check-column? #f
                                      #:last-line -1
                                      #:delta 0)))
  (when tail-commenting (fail-no-comment-group tail-commenting))
  (unless (null? rest-l)
    (error "had leftover items" rest-l))
  (define top (lists->syntax (cons 'top (bars-insert-alts gs))))
  (add-raw-tail top tail-raw))

;; Parse a sequence of groups (top level, in opener-closer, or after `:`)
;;   consuming the closer in the case of opener-closer context.
;; Returns: the list of groups
;;          remaining tokens after a closer
;;          line of last consumed (possibly closer)
;;          delta of last consumed
;;          last token consumed (if a closer)
;;          pending group-comment token from end
;;          pending raw whitespace
(define (parse-groups l sg)
  (define (check-no-commenting #:tail-ok? [tail-ok? #f])
    (define commenting (or (group-state-commenting sg)
                           (and (not tail-ok?) (group-state-tail-commenting sg))))
    (when commenting
      (fail-no-comment-group commenting)))
  (define (done end-t)
    (check-no-commenting #:tail-ok? #t)
    (values null l (group-state-last-line sg) (group-state-delta sg) end-t
            (group-state-tail-commenting sg)
            (group-state-raw sg)))
  (define (check-column t column)
    (when (group-state-check-column? sg)
      (unless (= column (group-state-column sg))
        (fail t "wrong indentation"))))
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
       [(eq? (token-name t) 'group-comment)
        ;; column doesn't matter
        (define-values (rest-l last-line delta raw)
          (next-of (cdr l) (token-line t) (group-state-delta sg) (cons t (group-state-raw sg))))
        (cond
          [(and ((token-line t) . > . (group-state-last-line sg))
                (next-line? rest-l last-line))
           ;; structure comment is on its own line, so it comments out next group
           (check-no-commenting)
           (parse-groups rest-l (struct-copy group-state sg
                                             [last-line last-line]
                                             [delta delta]
                                             [tail-commenting t]
                                             [raw raw]))]
          [else
           (fail t "misplaced group comment")
           (parse-groups rest-l (struct-copy group-state sg
                                             [last-line last-line]
                                             [delta delta]
                                             [raw raw]))])]
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
             (next-of l (group-state-last-line sg) (group-state-delta sg) (group-state-raw sg)))
           (parse-groups next-l (struct-copy group-state sg
                                             [last-line last-line]
                                             [delta delta]
                                             [raw raw]))]
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
              (define-values (rest-l last-line delta raw)
                (next-of (cdr l) (token-line t) (group-state-delta sg) (cons t (group-state-raw sg))))
              ;; In top level or immediately in opener-closer: 
              (parse-groups rest-l (struct-copy group-state sg
                                                [check-column? (next-line? rest-l last-line)]
                                                [last-line last-line]
                                                [comma-time? #f]
                                                [delta delta]
                                                [raw raw]))])]
          [(semicolon-operator)
           (when (group-state-paren-immed? sg)
             (fail t (format "misplaced semicolon (~a)"
                             "immdiately within parentheses or brackets")))
           (check-column t column)
           (define-values (rest-l last-line delta raw)
             (next-of (cdr l) (token-line t) (group-state-delta sg)  (cons t (group-state-raw sg))))
           (parse-groups rest-l (struct-copy group-state sg
                                             [check-column? (next-line? rest-l last-line)]
                                             [column (or (group-state-column sg) column)]
                                             [last-line last-line]
                                             [delta delta]
                                             [commenting (group-state-tail-commenting sg)]
                                             [tail-commenting #f]
                                             [raw raw]))]
          [else
           (when (group-state-comma-time? sg)
             (fail t (format "missing comma before new group (~a)"
                             "within parentheses or braces")))
           (define (check-block-mode)
             (when (eq? (group-state-block-mode sg) 'end)
               (fail t "no groups allowed after `}` in a block")))
           (case (token-name t)
             [(bar-operator)
              (define line (token-line t))
              (cond
                [(or (group-state-bar-closes? sg)
                     (eqv? line (group-state-bar-closes-line sg)))
                 (done #f)]
                [(eq? (group-state-block-mode sg) 'inside)
                 ;; Bar at the start of a group
                 (define same-line? (or (not (group-state-last-line sg))
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
                                #:line line
                                #:closer (column-next column)
                                #:bar-closes? #t
                                #:bar-closes-line line
                                #:delta (group-state-delta sg)
                                #:raw (if commenting
                                          null
                                          pre-raw)))
                 (define-values (gs rest-rest-l end-line end-delta end-t tail-commenting tail-raw)
                   (parse-groups rest-l (struct-copy group-state sg
                                                     [column (if same-line?
                                                                 (group-state-column sg)
                                                                 column)]
                                                     [check-column? #t]
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
                 (check-block-mode)
                 (if (not (group-state-block-mode sg))
                     (fail t "misplaced `|`")
                     (fail (group-state-block-mode sg) "unnecessary `:` before `|`"))])]
             [else
              ;; Parse one group, then recur to continue the sequence:
              (check-block-mode)
              (check-column t column)
              (define line (token-line t))
              (define pre-raw (group-state-raw sg))
              (define commenting (or (group-state-commenting sg)
                                     (group-state-tail-commenting sg)))
              (define-values (g rest-l group-end-line group-delta group-tail-commenting group-tail-raw)
                (parse-group l (make-state #:paren-immed? (group-state-paren-immed? sg)
                                           #:line line
                                           #:column column
                                           #:bar-closes? (group-state-bar-closes? sg)
                                           #:bar-closes-line (group-state-bar-closes-line sg)
                                           #:block-mode (group-state-block-mode sg)
                                           #:delta (group-state-delta sg)
                                           #:raw null)))
              (define-values (gs rest-rest-l end-line end-delta end-t tail-commenting tail-raw)
                (parse-groups rest-l (struct-copy group-state sg
                                                  [column (or (group-state-column sg) column)]
                                                  [check-column? (next-line? rest-l group-end-line)]
                                                  [last-line group-end-line]
                                                  [comma-time? (group-state-paren-immed? sg)]
                                                  [block-mode (if (and (is-block-pre-group? g)
                                                                       (not commenting))
                                                                  'end
                                                                  (group-state-block-mode sg))]
                                                  [delta group-delta]
                                                  [commenting #f]
                                                  [tail-commenting group-tail-commenting]
                                                  [raw (if commenting
                                                           (append group-tail-raw
                                                                   (cons (syntax-to-raw (datum->syntax #f g))
                                                                         pre-raw))
                                                           group-tail-raw)])))
              (values (if (or commenting
                              (null? g)) ; can happen due to a term comment
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

(define (is-block-pre-group? g)
  (and (pair? g)
       (pair? (car g))
       (tag? 'block (caar g))))

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
         (fail t "no terms allowed after `}` in a group")))
     ;; Consume a token
     (define (keep delta)
       (check-block-mode)
       (define-values (g rest-l end-line end-delta tail-commenting tail-raw)
         (parse-group (cdr l) (struct-copy state s
                                           [line line]
                                           [delta delta]
                                           [raw null]
                                           [block-mode #f])))
       (define elem (record-raw (token-value t) #f (state-raw s)))
       (values (cons elem g) rest-l end-line end-delta tail-commenting tail-raw))
     ;; Dispatch
     (cond
       [(line . > . (state-line s))
        ;; new line
        (case (token-name t)
          [(whitespace comment)
           (parse-group (cdr l) (struct-copy state s
                                             [line line]
                                             [delta 0]
                                             [raw (cons t (state-raw s))]))]
          [else
           ;; consume any structure comments that are on their own line:
           (define-values (group-commenting use-t use-l last-line delta raw)
             (get-own-line-group-comment t l (state-line s) (state-delta s) (state-raw s)))
           (define column (token-column use-t))
           (cond
             [(column . > . (state-column s))
              ;; More indented forms a nested block, but we require
              ;; `:` or `|` to indicate that it's ok to start an
              ;; indented block
              (cond
                [(eq? 'bar-operator (token-name use-t))
                 (parse-block #f l
                              #:block-mode 'inside
                              #:line (token-line t)
                              #:closer (token-column t)
                              #:bar-closes? #f
                              #:bar-closes-line #f
                              #:delta (state-delta s)
                              #:raw (state-raw s))]
                [else
                 (fail use-t "wrong indentation (or missing `:` on previous line)")])]
             [(and (state-paren-immed? s)
                   (eq? 'operator (token-name use-t)))
              ;; Treat an non-indented leading operator as a continuation of the group
              (when group-commenting
                (fail-no-comment-group group-commenting))
              (keep 0)]
             [else
              ;; using `done` here means that we leave any group comments in place;
              ;; not consuming inspected tokens is normally worrisome, but we'll parse
              ;; them at most one more time
              (done)])])]
       [else
        ;; Not a new line
        (case (token-name t)
          [(closer comma-operator semicolon-operator)
           (done)]
          [(identifier number literal operator)
           (keep (state-delta s))]
          [(block-operator)
           (check-block-mode)
           (parse-block t (cdr l)
                        #:line (token-line t)
                        #:closer (column-half-next (state-column s))
                        #:delta (state-delta s)
                        #:raw (state-raw s)
                        #:bar-closes? #f
                        #:bar-closes-line (state-bar-closes-line s))]
          [(bar-operator)
           (define line (token-line t))
           (cond
             [(or (state-bar-closes? s)
                  (eqv? line (state-bar-closes-line s)))
              (done)]
             [else
              (check-block-mode)
              (parse-block #f l
                           #:block-mode 'inside
                           #:line line
                           #:closer (token-column t)
                           #:bar-closes? #f
                           #:bar-closes-line #f
                           #:delta (state-delta s)
                           #:raw (state-raw s))])]
          [(opener)
           (check-block-mode)
           (define-values (closer tag paren-immed?)
             (case (token-e t)
               [("(") (values ")" 'parens #t)]
               [("{") (values "}" 'block* #f)]
               [("[") (values "]" 'brackets #t)]
               [else (error "unknown opener" t)]))
           (define pre-raw (state-raw s))
           (define ephemeral?
             (and (eq? tag 'block*)
                  (state-block-mode s)))
           (define-values (group-commenting next-l last-line delta raw)
             (next-of/commenting (cdr l) line (state-delta s) (if ephemeral?
                                                                  (cons t pre-raw)
                                                                  null)))
           (when (eq? tag 'block*)
             (unless (or (state-block-mode s)
                         (and (pair? next-l)
                              (let ([next-t (car next-l)])
                                (or (eq? (token-name next-t) 'bar-operator)
                                    (and (eq? (token-name next-t) 'opener)
                                         (equal? "{" (token-e t)))))))
               (fail t "misplaced block")))
           (define sub-column
             (if (pair? next-l)
                 (+ (token-column (car next-l)) (state-delta s))
                 (column-next (+ (token-column t) (state-delta s)))))
           (define-values (gs rest-l close-line close-delta end-t never-tail-commenting group-tail-raw)
             (parse-groups next-l (make-group-state #:closer (make-closer-expected closer t)
                                                    #:paren-immed? paren-immed?
                                                    #:block-mode (cond
                                                                   [paren-immed? #f]
                                                                   [(token? (state-block-mode s)) (state-block-mode s)]
                                                                   [else 'inside])
                                                    #:column sub-column
                                                    #:last-line last-line
                                                    #:delta delta
                                                    #:commenting group-commenting
                                                    #:raw raw)))
           (define-values (g rest-rest-l end-line end-delta tail-commenting tail-raw)
             (parse-group rest-l (struct-copy state s
                                              [line close-line]
                                              [delta close-delta]
                                              [block-mode (if (eq? tag 'block*) 'end #f)]
                                              [raw null])))
           (values (cons (if ephemeral?
                             (add-tail-raw-to-prefix
                              group-tail-raw
                              (cons tag gs))
                             (add-raw-to-prefix
                              t pre-raw #:tail group-tail-raw
                              (add-span-srcloc
                               t end-t
                               (if (eq? tag 'block*)
                                   (tag-as-block gs)
                                   (cons tag gs)))))
                         g)
                   rest-rest-l
                   end-line
                   end-delta
                   tail-commenting
                   tail-raw)]
          [(whitespace comment continue-operator)
           (define-values (next-l line delta raw)
             (next-of l (state-line s) (state-delta s) (state-raw s)))
           (parse-group next-l (struct-copy state s
                                            [line line]
                                            [delta delta]
                                            [raw raw]))]
          [(group-comment)
           (fail t "misplaced group comment")
           (parse-group (cdr l) s)]
          [else
           (error "unexpected" t)])])]))

(define (parse-block t l
                     #:line line
                     #:closer closer
                     #:bar-closes? [bar-closes? #f]
                     #:bar-closes-line [bar-closes-line #f]
                     #:delta in-delta
                     #:raw in-raw
                     #:block-mode [block-mode t])
  (define-values (group-commenting next-l last-line delta raw)
    (next-of/commenting l line in-delta null))
  (cond
    [(pair? next-l)
     (define next-t (car next-l))
     (define-values (indent-gs rest-l end-line end-delta end-t tail-commenting tail-raw)
       (parse-groups next-l
                     (make-group-state #:closer closer
                                       #:column (+ (token-column next-t) delta)
                                       #:last-line last-line
                                       #:bar-closes? bar-closes?
                                       #:bar-closes-line bar-closes-line
                                       #:block-mode block-mode
                                       #:delta delta
                                       #:commenting group-commenting
                                       #:raw raw)))
     (values (list (add-raw-to-prefix
                    t in-raw
                    (add-span-srcloc
                     t end-t #:alt next-t
                     (tag-as-block indent-gs))))
             rest-l
             end-line
             end-delta
             tail-commenting
             tail-raw)]
    [else
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
          (null? (cdr gs))
          (let ([g (car gs)])
            (and (pair? g)
                 (tag? 'group (car g))
                 (pair? (cdr g))
                 (null? (cddr g))
                 (let ([b (cadr g)])
                   (and (pair? b)
                        (tag? 'block* (car b))
                        b)))))
     => (lambda (b)
          ;; replace temporary 'block*
          (move-pre-raw (caar gs)
                        (move-post-raw (car b)
                                       (tag-as-block (cdr b)))))]
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

(define (add-span-srcloc start-t end-t l #:alt [alt-start-t #f])
  (define (add-srcloc l loc)
    (cons (let ([stx (datum->syntax #f (car l) loc stx-for-original-property)])
            (if (syntax? start-t)
                (let* ([stx (syntax-property-copy stx start-t 'raw)]
                       [stx (syntax-property-copy stx start-t 'raw-prefix)])
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

(define (syntax-property-copy dest src prop)
  (define v (syntax-property src prop))
  (if v
      (syntax-property dest prop v)
      dest))

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
         (datum->syntax #f new-l a stx-for-original-property)
         (datum->syntax #f new-l))]
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
(define (next-of l last-line delta raw)
  (cond
    [(null? l) (values null (or last-line 0) delta raw)]
    [else
     (define t (car l))
     (case (token-name t)
       [(whitespace comment)
        (next-of (cdr l) last-line delta (cons (car l) raw))]
       [(continue-operator)
        (define line (token-line t))
        ;; a continue operator not followed only by whitespace and
        ;; comments is just treated as whitespace
        (define-values (next-l next-raw)
          (let loop ([l (cdr l)] [raw raw])
            (cond
              [(null? l) (values null raw)]
              [else (case (token-name (car l))
                      [(whitespace comment) (loop (cdr l) (cons (car l) raw))]
                      [else (values l raw)])])))
        (cond
          [(and (pair? next-l)
                (= line (token-line (car next-l))))
           ;; like whitespace:
           (next-of next-l last-line delta next-raw)]
          [else
           (next-of next-l
                    ;; whitespace-only lines don't count, so next continues
                    ;; on the same line by definition:
                    #f
                    (+ (if (or (not last-line) (= line last-line))
                           delta
                           0)
                       (token-column t) 1)
                    next-raw)])]
       [else
        (define line (token-line t))
        (values l
                (or last-line line)
                (if (or (not last-line) (= line last-line))
                    delta
                    0)
                raw)])]))

(define (next-of/commenting l last-line delta raw)
  (define-values (rest-l rest-last-line rest-delta rest-raw)
    (next-of l last-line delta raw))
  (cond
    [(pair? rest-l)
     (define-values (group-commenting use-t use-l last-line delta raw)
       (get-own-line-group-comment (car rest-l) rest-l rest-last-line rest-delta rest-raw))
     (values group-commenting use-l last-line delta raw)]
    [else
     (values #f rest-l rest-last-line rest-delta rest-raw)]))

;; t is at the start of l on input and output
(define (get-own-line-group-comment t l line delta raw)
  (let loop ([commenting #f] [t t] [l (cdr l)] [line line] [delta delta] [raw raw])
    (case (token-name t)
      [(group-comment)
       (cond
         [((token-line t) . > . line)
          (define-values (next-l last-line next-delta next-raw)
            (next-of l line delta (cons t raw)))
          (cond
            [(null? next-l) (fail-no-comment-group t)]
            [(next-line? next-l last-line)
             (when commenting (fail-no-comment-group commenting))
             (loop t (car next-l) (cdr next-l) last-line next-delta next-raw)]
            [else
             ;; `t` is still in `next-raw`, but this is going to lead to an error
             (values commenting t (cons t next-l) last-line next-delta next-raw)])]
         [else
          (fail t "misplaced group comment")])]
      [else
       (values commenting t (cons t l) line delta raw)])))

(define (next-line? l last-line)
  (and (pair? l)
       ((token-line (car l)) . > . last-line)))

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
  (if (integer? c)
      (add1 c)
      (add1 (inexact->exact (floor c)))))

(define (column-half-next c)
  (if (integer? c)
      (+ c 0.5)
      (column-next c)))

(define (token-raw t)
  (define value (token-value t))
  (or (syntax-property value 'raw)
      (syntax-e value)))

(define (record-raw stx t pre-raw)
  (define stx+raw (if t
                      (syntax-property stx 'raw (cons (token-raw t)
                                                      (or (syntax-property stx 'raw) '())))
                      (if (syntax-property stx 'raw)
                          stx
                          (syntax-property stx 'raw ""))))
  (if (null? pre-raw)
      stx+raw
      (syntax-property stx+raw 'raw-prefix (cons (raw-tokens->raw pre-raw)
                                                 (or (syntax-property stx 'raw-prefix) '())))))
(define (add-raw-tail top raw)
  (if (null? raw)
      top
      (datum->syntax top
                     (cons (syntax-property (car (syntax-e top)) 'raw-tail (raw-tokens->raw raw))
                           (cdr (syntax-e top)))
                     top
                     top)))

(define (add-pre-raw stx pre-raw)
  (if (null? pre-raw)
      stx
      (syntax-property stx 'raw-prefix (raw-tokens->raw pre-raw))))

(define (move-pre-raw from-stx to)
  (define pre-raw (and (syntax? from-stx)
                       (syntax-property from-stx 'raw-prefix)))
  (cond
    [pre-raw
     (define a (car to))
     (cons (syntax-property a 'raw (cons pre-raw
                                         (or (syntax-property a 'raw) '())))
           (cdr to))]
    [else to]))

(define (move-post-raw from-stx to)
  (define post-raw (and (syntax? from-stx)
                        (syntax-property from-stx 'raw-tail)))
  (cond
    [post-raw
     (define a (datum->syntax #f (car to)))
     (cons (syntax-property a 'raw-tail (cons (or (syntax-property a 'raw-tail) '())
                                              post-raw))
           (cdr to))]
    [else to]))

(define (raw-tokens->raw pre-raw)
  (for/list ([raw-t (in-list (reverse pre-raw))])
    (if (token? raw-t)
        (token-raw raw-t)
        raw-t)))

(define (add-raw-to-prefix t pre-raw l #:tail [post-raw #f])
  (cons (let ([stx (record-raw (car l) t pre-raw)])
          (if post-raw
              (syntax-property stx 'raw-tail (cons (or (syntax-property stx 'raw-tail) '())
                                                   (raw-tokens->raw post-raw)))
              stx))
        (cdr l)))

(define (add-tail-raw-to-prefix post-raw l)
  (cons (syntax-property (datum->syntax #f (car l)) 'raw-tail (raw-tokens->raw post-raw))
        (cdr l)))

;; ----------------------------------------

(define (parse-all in #:source [source (object-name in)])
  (define l (lex-all in fail #:source source))
  (if (null? l)
      eof
      (parse-top-groups l)))
  
(module+ main
  (require racket/cmdline
           "print.rkt")

  (define show-raw? #f)

  (define (parse-all* in)
    (port-count-lines! in)
    (define e (parse-all in))
    (unless (eof-object? e)
      (cond
        [show-raw?
         (for ([s (syntax->list e)])
           (printf "#|\n~a\n|#\n" (shrubbery-syntax->string s))
           (pretty-write (syntax->datum s)))]
        [else
         (pretty-write
          (syntax->datum e))])))

  (command-line
   #:once-each
   [("--recover") "Continue parsing after an error"
                  (current-recover-mode #t)]
   [("--raw") "Show raw strings when printing"
              (set! show-raw? #t)]
   #:args file
   (if (null? file)
       (parse-all* (current-input-port))
       (for-each (lambda (file)
                   (call-with-input-file*
                    file
                    parse-all*))
                 file))))
