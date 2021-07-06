#lang racket/base
(require "lex.rkt"
         syntax-color/racket-lexer
         racket/class
         racket/list)

;; TODO:
;;  \\

(provide shrubbery-lexer
         shrubbery-indent)

(define (shrubbery-lexer in pos status)
  (lex/status in pos status racket-lexer/status))

(define (line-start t pos)
  (send t paragraph-start-position (send t position-paragraph pos #t)))

(define (shrubbery-indent t pos)
  (define start (line-start t pos))
  (define current (get-current-indent t start))
  (case (send t classify-position (+ start current))
    [(parenthesis)
     (indent-like-opener t start current)]
    [(bar-operator)
     (indent-like-enclosing-group t start current #:as-bar? #t)]
    [else
     (indent-like-enclosing-group t start current)]))

(define (indent-like-enclosing-group t start current #:as-bar? [as-bar? #f])
  (define tabs (remove-dups (indentation-candidates t (sub1 start) #:as-bar? as-bar?)))
  (define next-tabs (memv current tabs))
  (cond
    [(and next-tabs (pair? (cdr next-tabs)))
     (cadr next-tabs)]
    [(null? tabs) 0]
    [else (car tabs)]))

(define (indent-like-opener t start current)
  (define-values (s e) (send t get-token-range (+ start current)))
  (define o-s (send t backward-match e 0))
  (cond
    [o-s
     (define o-line (line-start t o-s))
     ;; line up with indentation position of opener's group
     (or (and (positive? o-s)
              (get-block-indent t (sub1 o-s) (- o-s o-line) o-line))
         (- o-s o-line))]
    [else
     ;; didn't find match, so treat like other tokens
     (indent-like-enclosing-group t start current)]))

;; Gets list of candiates with further-right candidates first
(define (indentation-candidates t pos #:as-bar? [as-bar? #f])
  ;; invariant: candidate is within limit
  (let loop ([pos pos] [candidate #f] [limit #f])
    (cond
      [(eqv? limit -1) null]
      [else
       (define (maybe-list v) (if (or (not limit) (v . <= . limit)) (list v) null))
       (define (loop* pos new-candidate)
         (loop pos
               (if (or (not limit)
                       (new-candidate . < . limit))
                   new-candidate
                   candidate)
               (min new-candidate (or limit new-candidate))))
       (define-values (s e) (send t get-token-range pos))
       (define s-line (line-start t pos))
       (define category (send t classify-position s))
       (case category
         [(white-space comment)
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
                ;; first position within parens/braces/brackets; if
                ;; indentation the bracket's group is before the bracket,
                ;; then "outdent" that far
                (define block-indent (get-block-indent t (sub1 s) (- s s-line) s-line))
                (define inside (- s s-line))
                (if (and block-indent (block-indent . < . inside))
                    (maybe-list (+ block-indent 2))
                    (maybe-list (+ inside 2)))])]
            [else
             ;; Found parenthesized while walking backward
             (define r (send t backward-match e 0))
             (cond
               [(not r) (if (zero? s)
                            (list (or candidate 0))
                            (loop* (sub1 s) (- s s-line)))]
               [(zero? r) (list 0)]
               [else
                (define r-line (line-start t r))
                (loop* (sub1 r) (- r r-line))])])]
         [(block-operator)
          (define block-s (if (zero? s)
                              0
                              (get-block-indent t (sub1 s) (- s s-line) s-line)))
          (define outers (if (zero? s)
                             null
                             (remove-larger (loop (sub1 s) #f limit)
                                            block-s)))
          (append (cond
                    [candidate (if (and (pair? outers)
                                        (eqv? candidate (car outers)))
                                   null
                                   (list candidate))]
                    [(pair? outers) (maybe-list (+ block-s (if as-bar? 1 2)))]
                    [else null])
                  outers)]
         [(bar-operator)
          (define-values (s-outers skip-bar? bar-limit)
            (if as-bar?
                (find-bar-outer-start t s s-line limit)
                (values (sub1 s) #f limit)))
          (define outers (cond
                           [(zero? s) null]
                           [(and as-bar? (not skip-bar?)) null]
                           [else (remove-larger (loop s-outers #f bar-limit)
                                                (- e s-line))]))
          (append (cond
                    [candidate (list candidate)]
                    [else null])
                  (cond
                    [(or (not candidate)
                         (and as-bar? (not skip-bar?)))
                     (maybe-list (- (if as-bar? s (+ e 1))
                                    s-line))]
                    [else null])
                  outers)]
         [(separator)
          (if (zero? s)
              (list (or candidate 0))
              (loop (sub1 s) candidate limit))]
         [else
          (cond
            [(zero? s) (list 0)]
            [else
             (loop* (sub1 s) (- (if as-bar? (add1 s) s) s-line))])])])))

;; find the current indentation of the block that starts at or before `pos`,
;; a long as the block continues (not nested) on `at-line`
(define (get-block-indent t pos candidate at-line)
  (let loop ([pos pos] [candidate candidate] [at-line at-line])
    (define pos-line (line-start t pos))
    (cond
      [(pos-line . < . at-line)
       candidate]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define s-line (line-start t pos))
       (define category (send t classify-position s))
       (case category
         [(white-space comment)
          (if (zero? s)
              (list (or candidate 0))
              (loop (sub1 s) candidate at-line))]
         [(parenthesis)
          (cond
            [(opener? (send t get-text (sub1 e) e))
             candidate]
            [else
             ;; Found parenthesized while walking backward
             (define r (send t backward-match e 0))
             (cond
               [(not r) (loop (sub1 s) (- s-line s) at-line)]
               [(zero? r) (list 0)]
               [else
                (define r-line (line-start t r))
                (loop (sub1 r) (- r r-line) r-line)])])]
         [(block-operator bar-operator separator) candidate]
         [else
          (if (zero? s)
              (list 0)
              (loop (sub1 s) (- s s-line) s-line))])])))

;; Returns a position before `pos` to continue search, whether to skip
;; the bar that starts at `pos`, and an upper bound on useful search
;; results where the next bar starts. If going backward from the same
;; line hits another bar operator, then the skipping result is true,
;; and the first result is just `pos`-1. Otherwise, skip to the start
;; of the line, because intermediate positions will not be allowed for
;; the bar.
(define (find-bar-outer-start t orig-pos at-line orig-limit)
  (let loop ([pos (sub1 orig-pos)] [at-line at-line])
    (define pos-line (line-start t pos))
    (cond
      [(pos-line . < . at-line)
       (values pos #f -1)]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define s-line (line-start t pos))
       (define category (send t classify-position s))
       (case category
         [(white-space comment)
          (if (zero? s)
              (values 0 #f -1)
              (loop (sub1 s) at-line))]
         [(parenthesis)
          (cond
            [(opener? (send t get-text (sub1 e) e))
             (values s #f -1)]
            [else
             ;; Found parenthesized while walking backward
             (define r (send t backward-match e 0))
             (cond
               [(not r) (loop (sub1 s) at-line)]
               [(zero? r) (values 0 #f -1)]
               [else
                (define r-line (line-start t r))
                (loop (sub1 r) r-line)])])]
         [(bar-operator)
          (define limit (- s s-line))
          (values (sub1 orig-pos) #t (min limit (or orig-limit limit)))]
         [else
          (if (zero? s)
              (values 0 #f -1)
              (loop (sub1 s) s-line))])])))

(define (opener? s)
  (member s '("(" "{" "[")))

(define (get-current-indent t start)
  (define e (send t last-position))
  (let loop ([start start])
    (cond
      [(= start e) 0]
      [else
       (define str (send t get-text start (add1 start)))
       (cond
         [(whitespace? str)
          (+ 1 (loop (add1 start)))]
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

     
