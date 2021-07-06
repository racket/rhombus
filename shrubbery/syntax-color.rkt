#lang racket/base
(require "lex.rkt"
         syntax-color/racket-lexer
         racket/class
         racket/list)

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
    [else
     (indent-like-enclosing-group t start current)]))

(define (indent-like-enclosing-group t start current)
  (define tabs (indentation-candidates t (sub1 start)))
  #;(log-error ">> ~s" tabs)
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
     (cond
       [(equal? "}" (send t get-text (+ start current) (+ start current 1)))
        ;; line up with indentation position of opener's group
        (or (and (positive? o-s)
                 (get-block-indent t (sub1 o-s) (- o-s o-line) o-line))
            (- o-s o-line))]
       [else
        ;; line with opener
        (- o-s o-line)])]
    [else
     ;; didn't find match, so treat like other tokens
     (indent-like-enclosing-group t start current)]))

;; Gets list of candiates with further-right candidates first
(define (indentation-candidates t pos)
  ;; invariant: candidate is within limit
  (let loop ([pos pos] [candidate #f] [limit #f])
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
      [(white-space)
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
                         (loop* (sub1 s) (- s-line s)))]
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
                 [(pair? outers) (maybe-list (+ block-s 2))]
                 [else null])
               outers)]
      [(bar-operator)
       (define outers (if (zero? s)
                          null
                          (remove-larger (loop (sub1 s) #f limit)
                                         (- e s-line))))
       (append (cond
                 [candidate (list candidate)]
                 [else (maybe-list (- (+ e 1) s-line))])
               outers)]
      [(separator)
       (if (zero? s)
           (list (or candidate 0))
           (loop (sub1 s) #f limit))]
      [else
       (cond
         [(zero? s) (list 0)]
         [else
          (loop* (sub1 s) (- s s-line))])])))

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
         [(white-space)
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
               [(not r) (loop (sub1 s) (- s-line s))]
               [(zero? r) (list 0)]
               [else
                (define r-line (line-start t r))
                (loop (sub1 r) (- r r-line) r-line)])])]
         [(block-operator bar-operator separator) candidate]
         [else
          (if (zero? s)
              (list 0)
              (loop (sub1 s) (- s s-line) s-line))])])))

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
