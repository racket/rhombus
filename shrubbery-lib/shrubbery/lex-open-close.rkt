#lang racket/base
(require racket/port
         "lex.rkt"
         "lex-comment.rkt"
         "private/column.rkt"
         "variant.rkt")

(provide lex/open-close/status
         lex/open-close-nested-status?
         lex/open-close-dont-stop-status?)

;; This lexer adds a layer to `lex/comment/status`, so it reports
;; comment ranges as well as invisible parentheses.

;; To report invisible parentheses, we imitate "parse.rkt"
;; just enough to determine where groups and group sequences
;; start and end. The current parse state is represented as
;; an explicit stack, which has the benefit that it's easy to
;; inspect the stack to derive context-sensitive properties, such
;; as the `|` sequences that are around the current term.

;; Unlike normal color lexing, we need to look ahead in the token
;; stream to determine whether the current token needs invisible
;; closes. As a result, it's possible for lexing work to be done ---
;; but at most twice, since we only need to look ahead on non-white
;; token, and we don't need a lookahead when revisinting whitespace
;; tokens. (Actually, `|` lookahead can turn that into thrice, in
;; some cases --- but still not an arbitrary number of times.)

;; Represents the lexer state, where `status` is the state from
;; `lex/comment/status`:
(struct open-close-tracked (status
                            stack
                            lookedahead)
  #:prefab)

;; stack element for parsing within openers
(struct opened (opener) #:prefab)

;; stack element when inside `()`, `[]`, or `{}`
(struct comma-separated opened () #:prefab)

;; stack element when inside `''` or `«»`
(struct semicolon-separated opened () #:prefab)

;; stack element when inside `@{}`
(struct unseparated opened () #:prefab)

;; stack element when inside a block
(struct multi (line column started) #:prefab)

;; stack element when inside a single-group context
(struct single () #:prefab)

;; stack element representing a `|` that starts a sequence of alternatives;
;; shouldn't appear by itself at the head of the stack, but instead
;; always uder a `multi?` --- except when trasitioning from a block
;; to alts mode and the next non-whitespace/comment token is `|`
(struct alts (line column) #:prefab)

(define open-tag 'invisible-open-count)
(define close-tag 'invisible-close-count)

(define (lex/open-close/status in pos status0 racket-lexer/status #:variant [variant default-variant])
  (define-values (orig-status inner-status)
    (cond
      [(open-close-tracked? status0)
       (values status0 (open-close-tracked-status status0))]
      [else
       (values (open-close-tracked status0 null 0)
               status0)]))
  (define previous-lookedahead (open-close-tracked-lookedahead orig-status))

  ;; Here's where we call `lex/comment/status`, which in turn calls
  ;; `lex/status` to perform the main lexing task:
  (define-values (tok type paren start-pos end-pos backup new-inner-status)
    (lex/comment/status in pos inner-status racket-lexer/status #:variant variant))

  (define new-status
    (struct-copy open-close-tracked orig-status
                 [status new-inner-status]
                 [lookedahead (max 0
                                   (- previous-lookedahead (if (and end-pos start-pos)
                                                               (- end-pos start-pos)
                                                               0)))]))

  ;; We may add to `type` and update our state, but we'll return the rest
  ;; of result from `lex/comment/status` as-is:
  (define (finish type status)
    (values tok type paren start-pos end-pos (max backup previous-lookedahead) status))

  (define (add type key n)
    (if (hash? type)
        (hash-set type key (+ n (hash-ref type key 0)))
        (hash 'type type key n)))

  (define (token-name* tok)
    (and (token? tok) (token-name tok)))
  (define (token-line* tok)
    (and (token? tok) (token-line tok)))
  (define (token-column* tok)
    (and (token? tok)
         (column-floor (token-column tok))))

  (define (not-line-sensitive? status)
    (let loop ([stack (open-close-tracked-stack status)])
      (cond
        [(null? stack) #f]
        [(and (semicolon-separated? (car stack))
              (equal? "«" (opened-opener (car stack))))
         (or (null? (cdr stack))
             (null? (cddr stack))
             (not (and (semicolon-separated? (caddr stack))
                       (equal? "'" (opened-opener (caddr stack)))))
             (loop (cdr stack)))]
        [else (loop (cdr stack))])))

  (define (drop-alts stack)
    (if (and (pair? stack)
             (alts? (car stack)))
        (cdr stack)
        stack))

  ;; Returns `(values tok comma-before? semicolon-before? status)` where `tok`
  ;; is the next non-whitespace, non-comma/semicolon token, and the
  ;; middle results indicates whether a comma/semicolon appears before that.
  ;; An updated status record is returned, and that update records how far
  ;; we had to look ahead.
  (define (peek-relevant status)
    (define in* (peeking-input-port in))
    (let loop ([comma-before? #f]
               [semicolon-before? #f]
               [lookedahead 0]
               [inner-status (open-close-tracked-status status)])
      (define-values (tok type paren start-pos end-pos backup new-inner-status)
        (lex/comment/status in* pos inner-status racket-lexer/status))
      (let ([lookedahead (+ lookedahead (if (and end-pos start-pos)
                                            (- end-pos start-pos)
                                            0))])
        (case (token-name* tok)
          [(whitespace comment group-comment)
           (loop comma-before? semicolon-before? lookedahead new-inner-status)]
          [(comma-operator)
           (loop #t semicolon-before? lookedahead new-inner-status)]
          [(semicolon-operator)
           (loop comma-before? #t lookedahead new-inner-status)]
          [(at-content)
           ;; treat like a comma and/or semicolon
           (loop #t #t lookedahead new-inner-status)]
          [else
           (values tok
                   comma-before?
                   semicolon-before?
                   (struct-copy open-close-tracked status
                                [lookedahead (+ (open-close-tracked-lookedahead status)
                                                lookedahead)]))]))))

  (define (add-opens type status)
    ;; `tok` must be something that can start a group
    (define stack (open-close-tracked-stack status))
    (define frame (and (pair? stack) (car stack)))
    (cond
      [(comma-separated? frame)
       (values (add type open-tag 1) (struct-copy open-close-tracked status
                                                  [stack (cons (single)
                                                               stack)]))]
      [(unseparated? frame)
       (values type status)]
      [(or (semicolon-separated? frame)
           (not frame))
       (values (add type open-tag (if (not frame)
                                      1 ;; avoid implicit open that spans the whole input
                                      2))
               (struct-copy open-close-tracked status
                            [stack (cons (multi (token-line* tok)
                                                (token-column* tok)
                                                2)
                                         stack)]))]
      [(not-line-sensitive? status)
       (cond
         [(or (single? frame) (multi? frame))
          (values type status)]
         [else (error "unexpected frame in whitespace-insensitive mode" frame)])]
      [(single? frame)
       (values type status)]
      [(multi? frame)
       (cond
         [(or (eqv? (token-line* tok) (multi-line frame))
              (not (token-line* tok)))
          (if (= (multi-started frame) 2)
              (values type status)
              (values (add type open-tag (- 2 (multi-started frame)))
                       (struct-copy open-close-tracked status
                                    [stack (cons (struct-copy multi frame
                                                              [started 2])
                                                 (cdr stack))])))]
         [else
          (define column (multi-column frame))
          (define t-column (token-column* tok))
          (cond
            [(and t-column
                  column
                  (t-column . column<? . column))
             (add-opens type (struct-copy open-close-tracked status
                                          [stack (drop-alts (cdr stack))]))]
            [(and t-column
                  column
                  (t-column . column>? . column)
                  (= (multi-started frame) 2))
             ;; this allows lines that are too indented, but it captures
             ;; correctly indented operator-continued lines
             (values type status)]
            [else
             (values (add type open-tag 1) status)])])]
      [else
       (error "unexpected frame adding opens" frame)]))

  (define (add-closes type in-status)
    (cond
      [(null? (open-close-tracked-stack in-status))
       ;; shortcut
       (values type in-status)]
      [else
       ;; peek ahead as needed to get a non-whitespace next token
       (define-values (next-tok comma-before? semicolon-before? status)
         (peek-relevant in-status))

       (let loop ([type type] [status status])
         (define stack (open-close-tracked-stack status))
         (define frame (and (pair? stack) (car stack)))
         (define (pop) (struct-copy open-close-tracked status
                                    [stack (drop-alts (cdr stack))]))
         (cond
           [(not frame) (values type status)]
           [else
            (case (token-name* next-tok)
              [(closer at-closer EOF)
               (cond
                 [(single? frame) (loop (add type close-tag 1) (pop))]
                 [(multi? frame) (loop (add type close-tag (if (null? (cdr stack))
                                                               1 ; avoid close for whole input
                                                               2))
                                       (pop))]
                 [(or (opened? frame) (not frame)) (values type status)]
                 [(alts? frame) (loop type (pop))]
                 [else (error "unrecognized frame at closer" frame)])]
              [(bar-operator)
               (define (consume-frames type stack to-stack)
                 (let loop ([type type] [stack stack])
                   (cond
                     [(eq? stack to-stack)
                      type]
                     [else
                      (define frame (car stack))
                      (define new-type
                              (cond
                                [(single? frame) (add type close-tag 1)]
                                [(multi? frame) (add type close-tag 2)]
                                [else type]))
                      (loop new-type (cdr stack))])))
               (cond
                 [(current-alts stack next-tok)
                  => (lambda (alts-stack)
                       (values (consume-frames type stack alts-stack)
                               status))]
                 [(current-block-for-alts stack next-tok)
                  => (lambda (multi-stack)
                       (values (consume-frames type stack multi-stack)
                               (struct-copy open-close-tracked status
                                            [stack (cons (alts (token-line* next-tok)
                                                               (token-column* next-tok))
                                                         (cdr stack))])))]
                 [else
                  (values type status)])]
              [else
               (cond
                 [(and comma-before? (single? frame))
                  (values (add type close-tag 1) (pop))]
                 [(and semicolon-before?
                       (multi? frame)
                       ((multi-started frame) . > . 1))
                  (loop (add type close-tag 1)
                        (struct-copy open-close-tracked status
                                     [stack (cons (multi (multi-line frame)
                                                         (multi-column frame)
                                                         1)
                                                  (cdr stack))]))]
                 [(single? frame)
                  (values type status)]
                 [(multi? frame)
                  (cond
                    [(or (not-line-sensitive? status)
                         (eqv? (token-line* next-tok) (multi-line frame))
                         (not (token-line* next-tok)))
                     (values type status)]
                    [else
                     (define column (multi-column frame))
                     (define t-column (token-column* next-tok))
                     (cond
                       [(and column
                             t-column
                             (t-column . column<? . column))
                        (loop (add type close-tag (multi-started frame)) (pop))]
                       [(and column
                             t-column
                             (t-column . column>? . column))
                        ;; see "too indented" in `add-opens`
                        (values type status)]
                       [else
                        (values (if (= (multi-started frame) 2)
                                    (add type close-tag 1)
                                    type)
                                (struct-copy open-close-tracked status
                                     [stack (cons (multi (token-line* next-tok)
                                                         (multi-column frame)
                                                         1)
                                                  (cdr stack))]))])])]
                 [(or (unseparated? frame)
                      (comma-separated? frame)
                      (semicolon-separated? frame))
                  (values type status)]
                 [else (error "unrecognized frame adding closers" frame)])])]))]))

  (define (start-block type in-status kind)
    (define-values (next-tok comma-before? semicolon-before? status)
      (peek-relevant in-status))
    (define stack (open-close-tracked-stack status))
    (define frame (and (pair? stack) (car stack)))
    (cond
      [(or (and (multi? frame)
                (multi-column frame)
                (token-column* next-tok)
                ((token-column* next-tok) . column<? . (multi-column frame)))
           (memq (token-name* next-tok) '(closer at-closer)))
       ;; treat block-starting `:` or `|` like an operator
       (add-closes type status)]
      [(and (eq? (token-name* next-tok) 'bar-operator)
            (eq? (token-name* tok) 'block-operator))
       ;; `|` immediately after `:`; treat the `:` like an operator
       (add-closes type status)]
      [else
       (values type
               (struct-copy open-close-tracked status
                            [stack (cons (multi (token-line* next-tok)
                                                (token-column* next-tok)
                                                0)
                                         (if (eq? kind 'alts)
                                             (cons (alts (token-line* tok)
                                                         (token-column* tok))
                                                   stack)
                                             stack))]))]))

  ;; Find an `alts` parse in the stack where `tok` (which is a `|`)
  ;; starts a new alternative for that set of alternatives
  (define (current-alts stack tok)
    (cond
      [(null? stack) #f]
      [else
       (define frame (car stack))
       (cond
         [(alts? frame)
          (define column (alts-column frame))
          (define t-column (token-column* tok))
          (cond
            [(or (eqv? (token-line* tok) (alts-line frame))
                 (column=? t-column column))
             stack]
            [(and t-column
                  column
                  (t-column . column>? . column))
             #f]
            [else (current-alts (cdr stack) tok)])]
         [(opened? frame) #f]
         [(single? frame) #f]
         [(multi? frame)
          (define column (multi-column frame))
          (define t-column (token-column* tok))
          (cond
            [(and (not (eqv? (token-line* tok) (multi-line frame)))
                  t-column
                  column
                  (t-column . column>? . column))
             #f]
            [else
             (current-alts (cdr stack) tok)])]
         [else
          (error "unpexpected frame looking for alts")])]))

  ;; Similar to `current-alts`, but find a block that is extended by
  ;; an `|` after the block body
  (define (current-block-for-alts stack tok)
    (let loop ([stack stack] [skipped 0])
      (cond
        [(null? stack) #f]
        [else
         (define frame (car stack))
         (cond
           [(multi? frame)
            (define column (multi-column frame))
            (define t-column (token-column* tok))
            (cond
              [(eqv? t-column column)
               (and (positive? skipped)
                    stack)]
              [(and t-column
                    column
                    (t-column . column>? . column))
               #f]
              [else
               (loop (cdr stack) (add1 skipped))])]
           [else #f])])))

  (define name (token-name* tok))

  #;
  (log-error "?? ~s ~s [~s,~s] @ ~s / ~s" name
             (and (token? tok) (token-e tok))
             (and (token? tok) (token-line tok))
             (and (token? tok) (token-column tok))
             (open-close-tracked-stack new-status)
             new-status)
  (case name
    [(#f EOF whitespace comment group-comment
      comma-operator semicolon-operator at-content)
     (finish type new-status)]
    [(opener at-opener)
     (define-values (open-type open-status) (add-opens type new-status))
     (define opener (token-e tok))
     (define done-status (struct-copy open-close-tracked open-status
                                      [stack (cons (case opener
                                                     [("(" "[" "{")
                                                      (comma-separated opener)]
                                                     [("'" "«")
                                                      (semicolon-separated opener)]
                                                     [else
                                                      (unseparated opener)])
                                                   (open-close-tracked-stack open-status))]))
     (finish open-type done-status)]
    [(closer at-closer)
     (define stack (open-close-tracked-stack new-status))
     (define open-status
       (cond
         [(and (pair? stack)
               (opened? (car stack)))
          (struct-copy open-close-tracked new-status
                       [stack (cdr stack)])]
         [else new-status]))
     (define-values (closed-type done-status) (add-closes type open-status))
     (finish closed-type done-status)]
    [(block-operator)
     (define-values (open-type open-status) (add-opens type new-status))
     (define-values (close-type close-status) (start-block open-type open-status 'block))
     (finish close-type close-status)]
    [(bar-operator)
     (define stack (open-close-tracked-stack new-status))
     (define frame (and (pair? stack) (car stack)))
     (cond
       [(opened? frame)
        ;; ok to start a group here
        (define-values (open-type open-status) (add-opens type new-status))
        #;(log-error "-|> ~s" open-status)
        (define-values (close-type close-status) (start-block open-type open-status 'alts))
        (finish close-type close-status)]
       [(alts? frame)
        ;; alts that continue a block
        (define-values (close-type close-status) (start-block type new-status 'more-alts))
        (finish close-type close-status)]
       [(current-alts stack tok)
        => (lambda (new-stack)
             (define open-status (struct-copy open-close-tracked new-status
                                              [stack new-stack]))
             (define-values (close-type close-status) (start-block type open-status 'more-alts))
             (finish close-type close-status))]
       [else
        ;; continues enclosing group
        (define-values (close-type close-status) (start-block type new-status 'alts))
        (finish close-type close-status)])]
    [else
     (define-values (open-type open-status) (add-opens type new-status))
     #;(log-error "--> ~s" open-status)
     (define-values (close-type close-status) (add-closes open-type open-status))
     (finish close-type close-status)]))

(define (lex/open-close-nested-status? status)
  (lex/comment-nested-status? (open-close-tracked-inner-status status)))

(define (lex/open-close-dont-stop-status? status)
  (lex/comment-dont-stop-status? (open-close-tracked-inner-status status)))

(define (open-close-tracked-inner-status status)
  (if (open-close-tracked? status)
      (open-close-tracked-status status)
      status))
