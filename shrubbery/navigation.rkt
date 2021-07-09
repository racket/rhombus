#lang racket/base
(require racket/class
         racket/list
         "lex.rkt"
         "private/edit-help.rkt")

;; Conventions are the same as in "indentation.rkt"

(provide shrubbery-grouping-position)

(define (shrubbery-grouping-position t pos limit-pos direction)
  (case direction
    [(backward)
     (define-values (s e) (skip-whitespace #:and-separators? #t t (sub1 pos) -1))
     (define category (send t classify-position s))
     (case category
       [(parenthesis)
        (or (send t backward-match e 0)
            e)]
       [(bar-operator block-operator) e]
       [else s])]
    [(forward)
     (define-values (s e) (skip-whitespace #:and-separators? #t t pos 1))
     (define category (send t classify-position s))
     (case category
       [(parenthesis)
        (or (send t forward-match s (send t last-position))
            s)]
       [(block-operator bar-operator)
        (define-values (next-s next-e) (skip-whitespace t e 1))
        (define start (line-start t next-s))
        (define delta (line-delta t start))
        (skip-to-shallower t e (col-of next-s start delta)
                           #:bar-stop-line (and (eq? category 'bar-operator)
                                                start))]
       [else e])]
    [(down)
     (define-values (s e) (skip-whitespace #:and-separators? #t t pos 1))
     (define category (send t classify-position s))
     (case category
       [(parenthesis)
        (and (opener? (send t get-text s e))
             e)]
       [(block-operator bar-operator)
        e]
       [else #f])]
    [(up)
     (define start (line-start t pos))
     (define-values (s e) (skip-whitespace #:and-separators? #t t (sub1 pos) -1
                                           #:stay-on-line start))
     (define category (send t classify-position s))
     (case category
       [(parenthesis)
        (if (opener? (send t get-text s e))
            s
            (start-of-group #:or-out? #t t e start))]
       [(block-operator bar-operator)
        (start-of-group #:or-out? #t t e start)]
       [else
        (start-of-group #:or-out? (eq? s pos) t s start)])]
    [else #f]))

(define (skip-whitespace t pos dir
                         #:and-separators? [and-separators? #f]
                         #:stay-on-line [stay-on-line #f])
  (define end-pos (send t last-position))
  (let loop ([pos pos] [stay-on-line stay-on-line])
    (define-values (s e) (send t get-token-range pos))
    (define (continue #:ok-to-change-line? [ok-to-change-line? #f])
      (define start (and stay-on-line (line-start t s)))
      (cond
        [(and stay-on-line
              (not ok-to-change-line?)
              (not (eqv? start stay-on-line)))
         (if (dir . < . 0)
             (send t get-token-range e)
             (send t get-token-range (sub1 s)))]
        [else
         (if (dir . < . 0)
             (if (zero? s)
                 (values s e)
                 (loop (- s 1) start))
             (if (eqv? e end-pos)
                 (values s e)
                 (loop e start)))]))
    (define category (send t classify-position s))
    (case category
      [(white-space comment)
       (continue)]
      [(continue-operator)
       (continue #:ok-to-change-line? #t)]
      [(separator)
       (if and-separators?
           (continue)
           (values s e))]
      [else (values s e)])))

;; find the end of a block as a column less than `col`, but
;; if `bar-stop-line` is given, also stop at a bar on that line
(define (skip-to-shallower t pos col
                           #:bar-stop-line [bar-stop-line #f])
  (define end-pos (send t last-position))
  (let loop ([pos pos] [last-e #f])
    (cond
      [(= pos end-pos) end-pos]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define category (send t classify-position s))
       (case category
         [(white-space comment continue-operator)
          (loop e last-e)]
         [(separator) s]
         [else
          (define start (line-start t s))
          (define delta (line-delta t start))
          (define new-col (col-of s start delta))
          (cond
            [(new-col . < . col) (or last-e s)]
            [(and (eq? category 'bar-operator)
                  (eqv? start bar-stop-line))
             (or last-e s)]
            [else
             (case category
               [(parenthesis)
                (define o-e (send t forward-match s end-pos))
                (if o-e
                    (loop o-e o-e)
                    s)]
               [else (loop e e)])])])])))

;; return the start of the group containing `pos`, but if `pos`
;; is already the start of the group and `or-out?1, return the start of the
;; enclosing group
(define (start-of-group t orig-pos at-start
                        #:or-out? or-out?)
  (define (finish last-pos)
    (or last-pos
        (if or-out?
            (start-of-enclosing-group t orig-pos)
            orig-pos)))
  (let loop ([pos orig-pos] [last-pos #f] [at-start at-start])
    (cond
      [(= pos 0) (or last-pos 0)]
      [else
       (define-values (s e) (send t get-token-range (sub1 pos)))
       (define category (send t classify-position s))
       (case category
         [(white-space comment)
          (define start (line-start t s))
          (if (eqv? start at-start)
              (loop s last-pos at-start)
              (finish last-pos))]
         [(continue-operator)
          (loop s last-pos (line-start t s))]
         [(block-operator bar-operator)
          (or last-pos s)]
         [(parenthesis)
          (define o-s (send t backward-match e 0))
          (cond
            [o-s ; => `s` is a closer
             (define start (line-start t e))
             (if (eqv? start at-start)
                 (loop o-s o-s (line-start t o-s))
                 (finish last-pos))]
            [else ; `s` is an opener
             (or last-pos s)])]
         [(separator)
          (finish last-pos)]
         [else
          (define start (line-start t s))
          (if (eqv? start at-start)
              (loop s s at-start)
              (finish last-pos))])])))

;; return the start of the block or parens containing the group containg `pos`
(define (start-of-enclosing-group t pos)
  (define start (line-start t pos))
  (define col (col-of pos start (line-delta t start)))
  (let loop ([pos pos] [last-pos #f])
    (cond
      [(= pos 0) 0]
      [else
       (define-values (s e) (send t get-token-range (sub1 pos)))
       (define category (send t classify-position s))
       (case category
         [(white-space comment continue-operator) (loop s last-pos)]
         [(parenthesis)
          (define o-s (send t backward-match e 0))
          (cond
            [o-s ; => `s` is a closer
             (loop o-s o-s)]
            [else ; `s` is an opener
             s])]
         [(bar-operator)
          ;; needs to be outdented relative to initial `pos`
          (define s-start (line-start t pos))
          (define s-col (col-of pos s-start (line-delta t s-start)))
          (cond
            [(s-col . < . col) last-pos]
            [else (loop s last-pos)])]
         [(block-operator)
          (define s-col (get-block-column t (sub1 s) #f (line-start t s)))
          (cond
            [(s-col . < . col) (or last-pos s)]
            [else (loop s last-pos)])]
         [else
          (loop s s)])])))
