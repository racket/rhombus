#lang racket/base
(require racket/class
         racket/list
         "lex.rkt"
         "private/edit-help.rkt"
         "private/paren.rkt"
         "private/delta-text.rkt")
         
;; Conventions:
;;   pos = arbitary position
;;   s, e = range positions
;;   start = start of a line
;;   delta = amount to add to a line due to `\` on preceding lines
;;   col, candidate, limit = virtual column, includes any relevant delta
;;   tab = indentation relative to start, does not include delta

(provide shrubbery-indentation
         shrubbery-range-indentation
         shrubbery-paren-matches
         shrubbery-quote-matches)

(define NORMAL-INDENT 2)
(define BAR-INDENT 0)

(define (shrubbery-indentation t pos
                               #:multi? [multi? #f]
                               #:always? [always? multi?])
  (define start (line-start t pos))
  (define current-tab (get-current-tab t start))
  (cond
    ;; If `always?` is #f, we got here by the Return key;
    ;; don't indent when just inserting new lines
    [(and (not always?)
          (or (zero? pos) ; shouldn't happen in a real editor!
              (= (line-start t (sub1 pos)) (sub1 pos))))
     current-tab]
    [else
     ;; tabbing only makes sense if the target line is not a continuation
     ;; or if it continues an empty line
     (define delta (or (line-delta t start #:unless-empty? #t) 0))
     (cond
       [(eqv? delta 0)
        (define (like-enclosing #:as-bar? [as-bar? #f]
                                #:as-operator? [as-operator? #f]
                                #:also-zero? [also-zero? #f])
          (indent-like-enclosing-group t start current-tab
                                       #:multi? multi?
                                       #:as-bar? as-bar?
                                       #:as-operator? as-operator?
                                       #:also-zero? also-zero?))
        (case (classify-position t (+ start current-tab))
          [(closer)
           (indent-like-parenthesis t start current-tab)]
          [(bar-operator)
           (like-enclosing #:as-bar? #t)]
          [(group-comment)
           (like-enclosing #:as-bar? (bar-after-group-comment? t (+ start current-tab) start)
                           #:also-zero? #t)]
          [(operator)
           (like-enclosing #:as-operator? #t)]
          [(at-content)
           ;; no indenting in `@` context
           current-tab]
          [else
           (like-enclosing)])]
       [else
        ;; don't change indentation for a continuation line
        current-tab])]))

(define (shrubbery-range-indentation t s e)
  (define s-line (send t position-paragraph s))
  (define e-line (let ([line (send t position-paragraph e)])
                   (if (and (s . < . e)
                            (= e (send t paragraph-start-position line)))
                       (sub1 line)
                       line)))
  (cond
    [(= s-line e-line)
     ;; use single-line mode
     (define pos (send t paragraph-start-position s-line))
     (define amt (shrubbery-indentation t pos #:always? #t))
     (define current-amt (get-current-tab t pos))
     (list (list (max 0 (- current-amt amt))
                 (make-string (max 0 (- amt current-amt)) #\space)))]
    [else
     (define changes (indent-interior t s-line e-line))
     (cond
       [(or (not changes)
            (for/and ([change (in-list changes)])
              (equal? change '(0 ""))))
        ;; compute indentation for the first non-empty line; as long as that
        ;; involves inserting space or deleting no more space than is
        ;; available in all lines, shift all the lines the same
        (define lines (get-non-empty-lines t s-line e-line))
        (cond
          [(null? lines) '()]
          [else
           (define (line-position line) (send t paragraph-start-position line))
           (define pos (line-position (car lines)))
           (define amt-or-multi-amt (shrubbery-indentation t pos #:multi? #t))
           (define amts (if (list? amt-or-multi-amt)
                            amt-or-multi-amt
                            (list amt-or-multi-amt)))
           (define current-amt (get-current-tab t pos))
           (or
            ;; try each possible shift:
            (for/or ([amt (in-list amts)])
              (cond
                [(current-amt . < . amt)
                 ;; insert in all lines
                 (define ins-str (make-string (- amt current-amt) #\space))
                 (for/list ([line (in-range s-line (add1 e-line))])
                   (list 0 (if (memv line lines) ins-str "")))]
                [(current-amt . > . amt)
                 (define delta (- current-amt amt))
                 (and (for/and ([line (in-list lines)])
                        (delta . <= . (get-current-tab t (send t paragraph-start-position line))))
                      (for/list ([line (in-range s-line (add1 e-line))])
                        (list (if (memv line lines) delta 0) "")))]
                [else #f]))
            ;; no change
            '())])]
       [else (cons '(0 "") changes)])]))

(define (indent-interior t s-line e-line)
  (cond
    [(= s-line e-line) '()]
    [else
     (define pos (send t paragraph-start-position (add1 s-line)))
     (define indent (shrubbery-indentation t pos
                                           #:multi? #t))
     (cond
       [(or (not (list? indent))
            (= 1 (length indent)))
        (define amt (if (pair? indent) (car indent) indent))
        (define current (get-current-tab t pos))
        (define one
          (if (current . < . amt)
              (list 0 (make-string (- amt current) #\space))
              (list (- current amt) "")))
        (define changes
          (indent-interior (if (= current amt)
                               t
                               (make-delta-text t pos (- amt current)))
                           (add1 s-line)
                           e-line))
        (and changes
             (cons one changes))]
       [else #f])]))

(define (indent-like-enclosing-group t start current-tab
                                     #:as-bar? [as-bar? #f]
                                     #:as-operator? [as-operator? #f]
                                     #:multi? [multi? #f]
                                     #:also-zero? [also-zero? #f]
                                     #:leftmost? [leftmost? #f])
  ;; candidates are sorted right (larger tab) to left (smaller tab)
  (define (add-zero l) (if also-zero? (add-zero-to-end l) l))
  (define (leftmost l) (if (and leftmost? (pair? l)) (list (last l)) l))
  (define candidates (remove-dups (indentation-candidates t (sub1 start)
                                                          #:as-bar? as-bar?
                                                          #:as-operator? as-operator?)))
  (define delta (line-delta t start))
  (define tabs  (leftmost
                 (add-zero
                  (for/list ([col (in-list candidates)]
                             #:when (col . >= . delta))
                    (- col delta)))))
  ;; if the current state matches a candidate tab, we'll
  ;; use the next one (to the left)
  (define next-tabs (memv current-tab tabs))
  (cond
    [multi?
     (if next-tabs
         (append next-tabs
                 (take tabs (- (length tabs) (length next-tabs))))
         tabs)]
    [(and next-tabs (pair? (cdr next-tabs)))
     (cadr next-tabs)]
    [(null? tabs) 0]
    [else
     ;; default to rightmost:
     (car tabs)]))

(define (indent-like-parenthesis t start current-tab)
  (define-values (s e) (send t get-token-range (+ start current-tab)))
  (define o-s (send t backward-match e 0))
  (define (own-line? t pos #:direction dir)
    (define para (send t position-paragraph pos))
    (if (eq? dir 'before)
        (only-whitespace-between? t
                                  (send t paragraph-start-position para)
                                  pos)
        (only-whitespace-between? t
                                  pos
                                  (send t paragraph-end-position para))))
  (cond
    [(and o-s ; => s is closer
          ;; opener and closer on their own lines?
          (own-line? t (add1 o-s) #:direction 'after)
          (own-line? t s #:direction 'before))
     (define o-start (line-start t o-s))
     (define o-delta (line-delta t o-start))
     (define col (col-of o-s o-start o-delta))
     ;; line up with indentation position of opener's group
     (define use-col
       (or (and (positive? o-s)
                (let ()
                  (define paren (send t get-text (sub1 e) e))
                  (define next-s (if (equal? paren "»")
                                     (skip-block-operator t (sub1 o-s))
                                     (sub1 o-s)))
                  (get-block-column t next-s col o-start
                                    #:for-outdent? #t)))
           col))
     (define s-delta (line-delta t start))
     (if (use-col . > . s-delta)
         (- use-col s-delta)
         0)]
    [else
     ;; didn't find match, so treat mostly like other tokens,
     ;; but use only the leftmost possibility
     (indent-like-enclosing-group t start current-tab
                                  #:leftmost? #t)]))

;; Gets list of candiates with further-right candidates first starting
;; search with the token that contains `pos` (inclusive on the left
;; edge of the token, starts before the line that we want to indent).
;; The search works going backwards to find enclosing groups.
(define (indentation-candidates t pos
                                #:as-bar? [as-bar? #f]
                                #:as-operator? [as-operator? #f])
  (let loop ([pos pos]
             ;; possible target column, depending on what's we
             ;; find by looking further back; for example, this
             ;; candidate will be discarded if we find another
             ;; identifier just before it
             [candidate #f]
             ;; filter out possibilities to the right (exclusive)
             ;; of the limit
             [limit #f]
             ;; saw a bar in this group?
             [bar-after? as-bar?]
             ;; can also indent by an extra step?
             [plus-one-more? as-operator?]
             ;; did we just skip over and armored sequence?
             [armored? #f])
    ;; helper
    (define (maybe-list col [plus-one-more? #f])
      (cond
        [(not col) null]
        [(or (not limit) (col . <= . limit))
         (if plus-one-more?
             (list (+ col NORMAL-INDENT) col)
             (list col))]
        [else null]))
    ;; helper: loops where the candidate also works as a refined limit
    (define (loop* pos new-candidate plus-one-more? armored?)
      (define candidate (if new-candidate
                            (min new-candidate (or limit new-candidate))
                            limit))
      (loop pos new-candidate candidate #f plus-one-more? armored?))
    (define (keep s #:armored? [armored? #f])
      (define start (line-start t s))
      (define delta (line-delta t start))
      (define new-candidate (and (not (and as-bar?
                                           (later-bar-same-line? t s start)))
                                 (col-of (if as-bar? (+ s BAR-INDENT) s) start delta)))
      (loop* (sub1 s)
             ;; don't forget the old candidate if the new candidate would
             ;; be too deeply indented
             (if (and new-candidate
                      (or (not candidate) (new-candidate . <= . limit)))
                 new-candidate
                 candidate)
             (and plus-one-more?
                  (or (eqv? new-candidate 0)
                      (= start (line-start t (sub1 s)))))
             armored?))
    (cond
      [(eqv? limit -1) null]
      [(negative? pos) (maybe-list candidate plus-one-more?)]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define category (let ([c (classify-position t s)])
                          (if (and (eq? c 'group-comment)
                                   as-bar?)
                              'comment
                              c)))
       (case category
         [(whitespace comment continue-operator)
          ;; we don't do anything special with continue-operator here,
          ;; because we avoid looking at line numbers, anyway, and `line-delta`
          ;; is responsible for computing continuation columns
          (loop (sub1 s) candidate limit bar-after? plus-one-more? armored?)]
         [(block-operator)
          (cond
            [armored?
             ;; indentation under the block is not valid
             (keep s)]
            [else
             ;; a block creates an indentation candidate that's
             ;; to the right of the enclosing group's indentation
             (define start (line-start t pos))
             (define delta (line-delta t start))
             (define block-col (if (zero? s)
                                   0
                                   (get-block-column t (sub1 s) (col-of s start delta) start)))
             (define next-s (sub1 s))
             ;; indentation under the block operator is valid unless armored
             (define next-candidate (col-of (add1 next-s) start delta))
             ;; a `|` cannot appear just after a `:`, so look before that block
             (define adj-block-col (if as-bar? (sub1 block-col) block-col))
             ;; look further outside this block, and don't consider anything
             ;; that would appear to be nested in the block:
             (define outer-candidates (if (or candidate
                                              (block-not-disallowed-empty? t pos start))
                                          (loop next-s next-candidate (min* adj-block-col limit) #f #f #f)
                                          ;; block is empty so far, so son't go outside it
                                          null))
             (append (cond
                       [(and bar-after? (not as-bar?))
                        null]
                       [candidate
                        ;; we already have something after `:`, so
                        ;; use its indentation
                        (maybe-list candidate plus-one-more?)]
                       [else
                        ;; we haven't found anything in the block, so
                        ;; indent as the first thing in the block
                        (maybe-list (+ block-col (if as-bar? BAR-INDENT NORMAL-INDENT)))])
                     outer-candidates)])]
         [else
          (cond
            [(and candidate
                  (eqv? candidate (if as-bar? BAR-INDENT 0))
                  (or (not limit) (limit . >= . (if as-bar? BAR-INDENT 0))))
             ;; already found minimal column, so stop here
             (maybe-list candidate plus-one-more?)]
            [else
             (case category
               [(opener)
                ;; we're inside parentheses, brackets, etc.
                (cond
                  [(and bar-after? (not as-bar?))
                   ;; opener followed by a bar: no more candidates
                   null]
                  [candidate (maybe-list candidate plus-one-more?)]
                  [(zero? s) (maybe-list NORMAL-INDENT)]
                  [else
                   (define start (line-start t pos))
                   (define delta (line-delta t start))
                   ;; first position within parens/braces/brackets; if
                   ;; indentation for the bracket's group is before the bracket,
                   ;; then "outdent" that far
                   (define col (col-of s start delta))
                   (define next-s (if (equal? (send t get-text s e) "«")
                                      (skip-block-operator t (sub1 s))
                                      (sub1 s)))
                   (define block-col (get-block-column t next-s col start
                                                       #:for-outdent? #t))
                   (if (and block-col (block-col . < . col))
                       (maybe-list (+ block-col NORMAL-INDENT))
                       (maybe-list (+ col NORMAL-INDENT)))])]
               [(closer)
                ;; found parenthesized while walking backward
                (define r (send t backward-match e 0))
                (cond
                  [(not r)
                   ;; matching open not found
                   (cond
                     [(zero? s) (maybe-list candidate)]
                     [else (keep s)])]
                  [(zero? r) null]
                  [(equal? "«" (send t get-text r (add1 r)))
                   (loop (sub1 r) candidate limit bar-after? plus-one-more? #t)]
                  [else (keep r)])]
               [(bar-operator)
                (define start (line-start t pos))
                ;; look back to see whether there's another bar on the
                ;; same line:
                (define-values (another-bar-start limit-pos)
                  (find-bar-same-line t s start as-bar?))
                (cond
                  [(or (and another-bar-start as-bar?)
                       (next-is-block-bracket? t e))
                   ;; don't treat the current bar as a source
                   ;; of indentation:
                   (loop (sub1 s) #f (min* s limit) #t #f #f)]
                  [as-bar?
                   (define delta (line-delta t start))
                   (define col (col-of s start delta))
                   (cond
                     [(not limit-pos) (maybe-list col)]
                     [else
                      ;; a new bar can line up with outer candidates
                      ;; beyond the found bar
                      (define b-start (line-start t limit-pos))
                      (define b-delta (line-delta t b-start))
                      (define b-col (col-of limit-pos b-start b-delta))
                      (append (maybe-list candidate)
                              (maybe-list col)
                              ;; outer candidates:
                              (loop (sub1 limit-pos)
                                    b-col
                                    (min* (min col (sub1 b-col)) limit)
                                    #t
                                    #f
                                    #f))])]
                  [else
                   ;; line up within bar or outside [another] bar
                   (define bar-column (col-of s start (line-delta t start)))
                   (append (maybe-list (or candidate (+ bar-column NORMAL-INDENT)) (and candidate plus-one-more?))
                           (if (or (not limit-pos)
                                   ;; if block is empty so far, so son't go outside it
                                   (not candidate))
                               null
                               ;; outer candidates
                               (loop (sub1 (or another-bar-start s)) #f (min* bar-column limit) #t #f #f)))])]
               [(semicolon-operator)
                (loop (sub1 s) candidate limit bar-after? #f #f)]
               [(comma-operator)
                (cond
                  [candidate null]
                  [else
                   (define i-pos (get-inside-start t pos))
                   (define start (line-start t i-pos))
                   (define delta (line-delta t start))
                   (maybe-list (col-of i-pos start delta) #f)])]
               [else
                (keep s)])])])])))

;; Returns:
;;   * the position of a bar before `orig-pos` with the same line
;;     start as reflected by `at-start` (but taking continuing lines
;;     into account), or #f if there's not one
;;   * the position where the bar's enclosing block starts, but only
;;     reliably if `as-bar?` and the first result is #f, or #f (all modes)
;;     to mean that not outer group is possible
(define (find-bar-same-line t orig-pos at-start as-bar?)
  (let loop ([pos (sub1 orig-pos)] [at-start at-start] [limit-pos (sub1 orig-pos)])
    (cond
      [(negative? pos) (values #f limit-pos)]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define category (classify-position t s))
       (case category
         [(whitespace comment)
          (loop (sub1 s) at-start limit-pos)]
         [(continue-operator)
          (loop (sub1 s) (line-start t s) limit-pos)]
         [else
          (define pos-start (line-start t pos))
          (cond
            [(pos-start . < . at-start) (values #f limit-pos)]
            [else
             (case category
               [(opener)
                (values #f #f)]
               [(closer)
                ;; Found parenthesized while walking backward
                (define r (send t backward-match e 0))
                (cond
                  [(not r) (loop (sub1 s) at-start s)]
                  [else (loop (sub1 r) (line-start t r) r)])]
               [(bar-operator)
                (cond
                  [(not as-bar?)
                   ;; keep looking back
                   (define-values (another-bar-pos limit-pos) (loop (sub1 s) at-start 0))
                   (values (or another-bar-pos s) limit-pos)]
                  [else
                   ;; don't need to find limit-pos
                   (values s 0)])]
               [else (loop (sub1 s) at-start s)])])])])))

(define (later-bar-same-line? t pos at-start)
  (define start (line-start t pos))
  (cond
    [(eqv? start at-start)
     (define-values (s e) (send t get-token-range pos))
     (define category (classify-position t s))
     (case category
       [(bar-operator) #t]
       [(opener)
        (define r (send t forward-match s (send t last-position)))
        (if r
            (later-bar-same-line? t r at-start)
            (later-bar-same-line? t e at-start))]
       [(closer) #f]
       [else
        (later-bar-same-line? t e at-start)])]
    [else #f]))

;; Skips back to an unmatched opener and returns the
;; position of the first thing after it (not counting
;; whitespace or comments)
(define (get-inside-start t pos)
  (let loop ([pos pos] [last-pos #f])
    (cond
      [(pos . <= . 0) 0]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define category (classify-position t s))
       (case category
         [(whitespace comment continue-operator)
          (loop (sub1 s) last-pos)]
         [(opener)
          (or last-pos e)]
         [(closer)
          ;; Found parenthesized while walking backward
          (define r (send t backward-match e 0))
          (loop (sub1 (or r s)) (or r s))]
         [else (loop (sub1 s) s)])])))

(define (skip-block-operator t pos)
  (define-values (s e) (send t get-token-range pos))
  (define category (classify-position t s))
  (case category
    [(whitespace comment continue-operator)
     (skip-block-operator t (sub1 s))]
    [(block-operator) (sub1 s)]
    [else pos]))

(define (bar-after-group-comment? t pos at-start)
  (let loop ([pos pos])
    (cond
      [(= pos (send t last-position)) #f]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define category (classify-position t s))
       (case category
         [(whitespace comment) (loop e)]
         [(bar-operator) (> (line-start t pos) at-start)]
         [else #f])])))

(define (next-is-block-bracket? t pos)
  (let loop ([pos pos])
    (cond
      [(= pos (send t last-position)) #f]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define category (classify-position t s))
       (case category
         [(whitespace comment) (loop e)]
         [(opener) (equal? (send t get-text s e) "«")]
         [else #f])])))

(define (get-non-empty-lines t s-line e-line)
  (let loop ([line s-line])
    (cond
      [(line . > . e-line) '()]
      [(only-whitespace-between? t
                                 (send t paragraph-start-position line)
                                 (send t paragraph-end-position line))
       (loop (add1 line))]
      [else (cons line (loop (add1 line)))])))

(define (remove-dups l)
  (cond
    [(null? l) null]
    [(null? (cdr l)) l]
    [(eqv? (car l) (cadr l)) (remove-dups (cdr l))]
    [else (cons (car l) (remove-dups (cdr l)))]))

(define (min* a b)
  (min a (or b a)))

(define (add-zero-to-end l)
  (let loop ([l l])
    (cond
      [(null? l) (list 0)]
      [(eqv? (car l) 0) l]
      [else (cons (car l) (loop (cdr l)))])))
