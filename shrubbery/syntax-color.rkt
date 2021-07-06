#lang racket/base
(require "lex.rkt"
         syntax-color/racket-lexer
         racket/class
         racket/list)

;; Conventions:
;;   pos = arbitary position
;;   s, e = range positions
;;   start = start of a line
;;   delta = amount to add to a line due to `\` on preceding lines
;;   col, candidate, limit = virtual column, includes any relevant delta
;;   tab = indentation relative to start, does not include delta

(provide shrubbery-lexer
         shrubbery-indent)

(define (shrubbery-lexer in pos status)
  (lex/status in pos status racket-lexer/status))

(define (shrubbery-indent t pos)
  (define start (line-start t pos))
  (define current-tab (get-current-tab t start))
  ;; tabbing only makes sense if the target line is not a continuation
  ;; or if it continues an empty line
  (define delta (line-delta t start #:require-empty? #t))
  (cond
    [(eqv? delta 0)
     (case (send t classify-position (+ start current-tab))
       [(parenthesis)
        (indent-like-opener t start current-tab)]
       [(bar-operator)
        (indent-like-enclosing-group t start current-tab #:as-bar? #t)]
       [else
        (indent-like-enclosing-group t start current-tab)])]
    [else
     current-tab]))

(define (indent-like-enclosing-group t start current-tab #:as-bar? [as-bar? #f])
  (define candidates (remove-dups (indentation-candidates t (sub1 start) #:as-bar? as-bar?)))
  (define delta (line-delta t start))
  (define tabs (for/list ([col (in-list candidates)]
                          #:when (col . >= . delta))
                 (- col delta)))
  (define next-tabs (memv current-tab tabs))
  (cond
    [(and next-tabs (pair? (cdr next-tabs)))
     (cadr next-tabs)]
    [(null? tabs) 0]
    [else (car tabs)]))

(define (indent-like-opener t start current-tab)
  (define-values (s e) (send t get-token-range (+ start current-tab)))
  (define o-s (send t backward-match e 0))
  (cond
    [o-s
     (define o-start (line-start t o-s))
     (define o-delta (line-delta t o-start))
     (define col (col-of o-s o-start o-delta))
     ;; line up with indentation position of opener's group
     (define use-col
       (or (and (positive? o-s)
                (get-block-column t (sub1 o-s) col o-start))
           col))
     (define s-delta (line-delta t start))
     (if (use-col . > . s-delta)
         (- use-col s-delta)
         0)]
    [else
     ;; didn't find match, so treat like other tokens
     (indent-like-enclosing-group t start current-tab)]))

;; Gets list of candiates with further-right candidates first
(define (indentation-candidates t pos #:as-bar? [as-bar? #f])
  ;; invariant: candidate is within limit
  (let loop ([pos pos] [candidate #f] [limit #f])
    (cond
      [(eqv? limit -1) null]
      [else
       (define (maybe-list col) (if (or (not limit) (col . <= . limit)) (list col) null))
       (define (loop* pos new-candidate)
         (loop pos
               (if (or (not limit)
                       (new-candidate . < . limit))
                   new-candidate
                   candidate)
               (min new-candidate (or limit new-candidate))))
       (define-values (s e) (send t get-token-range pos))
       (define category (send t classify-position s))
       (case category
         [(white-space comment continue-operator)
          (if (zero? s)
              (list (or candidate 0))
              (loop (sub1 s) candidate limit))]
         [(parenthesis)
          (cond
            [(opener? (send t get-text (sub1 e) e))
             ;; We're inside parentheses, brackets, etc.
             (cond
               [candidate (list candidate)]
               [(zero? s) (maybe-list 2)]
               [else
                (define start (line-start t pos))
                (define delta (line-delta t start))
                ;; first position within parens/braces/brackets; if
                ;; indentation the bracket's group is before the bracket,
                ;; then "outdent" that far
                (define col (col-of s start delta))
                (define block-col (get-block-column t (sub1 s) col start))
                (if (and block-col (block-col . < . col))
                    (maybe-list (+ block-col 2))
                    (maybe-list (+ col 2)))])]
            [else
             ;; Found parenthesized while walking backward
             (define r (send t backward-match e 0))
             (cond
               [(not r) (cond
                          [(zero? s) (list (or candidate 0))]
                          [else
                           (define start (line-start t pos))
                           (define delta (line-delta t start))
                           (loop* (sub1 s) (col-of s start delta))])]
               [(zero? r) (list 0)]
               [else
                (define start (line-start t r))
                (define delta (line-delta t start))
                (loop* (sub1 r) (col-of r start delta))])])]
         [(block-operator)
          (define start (line-start t pos))
          (define delta (line-delta t start))
          (define block-col (if (zero? s)
                                0
                                (get-block-column t (sub1 s) (col-of s start delta) start)))
          (define outer-canddiates (if (zero? s)
                                       null
                                       (remove-larger (loop (sub1 s) #f limit)
                                                      block-col)))
          (append (cond
                    [candidate (if (and (pair? outer-canddiates)
                                        (eqv? candidate (car outer-canddiates)))
                                   null
                                   (list candidate))]
                    [(pair? outer-canddiates) (maybe-list (+ block-col (if as-bar? 1 2)))]
                    [else null])
                  outer-canddiates)]
         [(bar-operator)
          (define start (line-start t pos))
          (define-values (s-outer-candidates skip-bar? bar-limit)
            (if as-bar?
                (find-bar-outer-start t s start limit)
                (values (sub1 s) #f limit)))
          (define outer-candidates
            (cond
              [(zero? s) null]
              [(and as-bar? (not skip-bar?)) null]
              [else
               (define delta (line-delta t start))
               (remove-larger (loop s-outer-candidates #f bar-limit)
                              (col-of e start delta))]))
          (append (cond
                    [candidate (list candidate)]
                    [else null])
                  (cond
                    [(or (not candidate)
                         (and as-bar? (not skip-bar?)))
                     (define delta (line-delta t start))
                     (maybe-list (col-of (if as-bar? s (+ e 1))
                                         start
                                         delta))]
                    [else null])
                  outer-candidates)]
         [(separator)
          (if (zero? s)
              (list (or candidate 0))
              (loop (sub1 s) candidate limit))]
         [else
          (cond
            [(zero? s) (list 0)]
            [else
             (define start (line-start t s))
             (define delta (line-delta t start))
             (loop* (sub1 s) (col-of (if as-bar? (add1 s) s) start delta))])])])))

;; find the current indentation of the block that starts at or before `pos`,
;; a long as the block continues (not nested) on the line at `at-start`
(define (get-block-column t pos candidate at-start)
  (let loop ([pos pos] [candidate candidate] [at-start at-start])
    (define pos-start (line-start t pos))
    (cond
      [(pos-start . < . at-start)
       candidate]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define category (send t classify-position s))
       (case category
         [(white-space comment continue-operator)
          (if (zero? s)
              (list (or candidate 0))
              (loop (sub1 s) candidate at-start))]
         [(parenthesis)
          (cond
            [(opener? (send t get-text (sub1 e) e))
             candidate]
            [else
             ;; Found parenthesized while walking backward
             (define r (send t backward-match e 0))
             (cond
               [(not r)
                (define start (line-start t pos))
                (define delta (line-delta t start))
                (loop (sub1 s) (col-of s start delta) at-start)]
               [(zero? r) (list 0)]
               [else
                (define start (line-start t r))
                (define delta (line-delta t start))
                (loop (sub1 r) (col-of r start delta) start)])])]
         [(block-operator bar-operator separator) candidate]
         [else
          (cond
            [(zero? s) (list 0)]
            [else
             (define start (line-start t pos))
             (define delta (line-delta t start))
             (loop (sub1 s) (col-of s start delta) start)])])])))

;; Returns a position before `pos` to continue search, whether to skip
;; the bar that starts at `pos`, and an upper bound on useful search
;; results where the next bar starts. If going backward from the same
;; line hits another bar operator, then the skipping result is true,
;; and the first result is just `pos`-1. Otherwise, skip to the start
;; of the line, because intermediate positions will not be allowed for
;; the bar.
(define (find-bar-outer-start t orig-pos at-start orig-limit)
  (let loop ([pos (sub1 orig-pos)] [at-start at-start])
    (define pos-start (line-start t pos))
    (cond
      [(pos-start . < . at-start)
       (values pos #f -1)]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define category (send t classify-position s))
       (case category
         [(white-space comment continue-operator)
          (if (zero? s)
              (values 0 #f -1)
              (loop (sub1 s) at-start))]
         [(parenthesis)
          (cond
            [(opener? (send t get-text (sub1 e) e))
             (values s #f -1)]
            [else
             ;; Found parenthesized while walking backward
             (define r (send t backward-match e 0))
             (cond
               [(not r) (loop (sub1 s) at-start)]
               [(zero? r) (values 0 #f -1)]
               [else
                (define start (line-start t r))
                (loop (sub1 r) start)])])]
         [(bar-operator)
          (define start (line-start t pos))
          (define delta (line-delta t start))
          (define limit (col-of s start delta))
          (values (sub1 orig-pos) #t (min limit (or orig-limit limit)))]
         [else
          (cond
            [(zero? s) (values 0 #f -1)]
            [else
             (define start (line-start t pos))
             (loop (sub1 s) start)])])])))

(define (opener? s)
  (member s '("(" "{" "[")))

(define (line-start t pos)
  (send t paragraph-start-position (send t position-paragraph pos #t)))

;; skip back over whitespace and comments to find `\`
(define (line-delta t start #:require-empty? [require-empty? #f])
  (let loop ([pos start])
    (cond
      [(eqv? pos 0) 0]
      [else
       (case (send t classify-position (sub1 pos))
         [(white-space comment)
          (define-values (s e) (send t get-token-range (sub1 pos)))
          (loop s)]
         [(continue-operator)
          ;; since we've only skipped comments and whitespace, this
          ;; continue operator applies
          (define-values (s e) (send t get-token-range (sub1 pos)))
          (define c-start (line-start t s))
          (and (or (not require-empty?)
                   (only-whitespace-between? t c-start s))
               (+ (- e c-start)
                  (line-delta t c-start #:require-empty? require-empty?)))]
         [else 0])])))

(define (only-whitespace-between? t s-pos e-pos)
  (case (send t classify-position s-pos)
    [(white-space comment continue-operator)
     (define-values (s e) (send t get-token-range s-pos))
     (or (e . >= . e-pos)
         (only-whitespace-between? t e e-pos))]
    [else #f]))

(define (col-of pos start delta)
  (+ (- pos start) delta))

(define (get-current-tab t start)
  (define e (send t last-position))
  (let loop ([pos start])
    (cond
      [(= pos e) 0]
      [else
       (define str (send t get-text pos (add1 pos)))
       (cond
         [(whitespace? str)
          (+ 1 (loop (add1 pos)))]
         [else 0])])))

(define (whitespace? str)
  (and (= (string-length str) 1)
       (char-whitespace? (string-ref str 0))
       (not (equal? str "\n"))))

(define (remove-larger l n)
  (cond
    [(null? l) null]
    [((car l) . > . n) (remove-larger (cdr l) n)]
    [else l]))

(define (remove-dups l)
  (cond
    [(null? l) null]
    [(null? (cdr l)) l]
    [(eqv? (car l) (cadr l)) (remove-dups (cdr l))]
    [else (cons (car l) (remove-dups (cdr l)))]))

(define (shift-indents t start cols)
  cols)

(define (shift-indent t start col)
  col)
