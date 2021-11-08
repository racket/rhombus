#lang racket/base
(require racket/class)

(provide classify-position
         line-start
         line-orig-start
         line-delta
         col-of
         only-whitespace-between?
         get-block-column
         get-current-tab
         end-of-current
         start-of-group
         skip-whitespace
         skip-hash-lang)

(define (classify-position t s)
  (define attribs (send t classify-position* s))
  (if (symbol? attribs)
      attribs
      (or (hash-ref attribs 'rhombus-type #f)
          (hash-ref attribs 'type #f))))

(define (line-start t pos)
  (send t paragraph-start-position (send t position-paragraph pos #t)))

;; backs up over continuing lines
(define (line-orig-start t pos #:limit [limit 0])
  (define start (line-start t pos))
  (let loop ([pos start] [start start])
    (cond
      [(pos . <= . limit) limit]
      [else
       (case (classify-position t (sub1 pos))
         [(whitespace comment)
          (define-values (s e) (send t get-token-range (sub1 pos)))
          (loop s start)]
         [(continue-operator)
          ;; since we've only skipped comments and whitespace, this
          ;; continue operator applies
          (define c-start (line-start t (sub1 pos)))
          (loop c-start c-start)]
         [else start])])))

(define (line-delta t start #:unless-empty? [unless-empty? #f])
  (let loop ([pos start])
    (cond
      [(eqv? pos 0) 0]
      [else
       (case (classify-position t (sub1 pos))
         [(whitespace comment)
          (define-values (s e) (send t get-token-range (sub1 pos)))
          (loop s)]
         [(continue-operator)
          ;; since we've only skipped comments and whitespace, this
          ;; continue operator applies
          (define-values (s e) (send t get-token-range (sub1 pos)))
          (define c-start (line-start t s))
          (define more-delta (line-delta t c-start #:unless-empty? unless-empty?))
          (and more-delta
               (if unless-empty?
                   (not (only-whitespace-between? t c-start s #:or-ws-like? #t))
                   #t)
               (+ (- e c-start) more-delta))]
         [else 0])])))

;; skip back over whitespace and comments to find `\`
(define (col-of pos start delta)
  (+ (- pos start) delta))

(define (only-whitespace-between? t s-pos e-pos
                                  #:or-ws-like? [or-ws-like? #f])
  (let loop ([pos s-pos])
    (or (pos . >= . e-pos)
        (and (case (classify-position t pos)
               [(whitespace) #t]
               [(comment continue-operator) or-ws-like?]
               [else #f])
             (let ()
               (define-values (s e) (send t get-token-range pos))
               (loop e))))))

;; find the current indentation of the block that starts at or before `pos`,
;; a long as the block continues (not nested) on the line at `at-start`;
;; if `for-outdent?` is true, don't treat leading operators as the start
(define (get-block-column t pos candidate at-start
                          #:for-outdent? [for-outdent? #t])
  (let loop ([pos pos] [candidate candidate] [at-start at-start])
    (define pos-start (line-start t pos))
    (cond
      [(pos-start . < . at-start)
       candidate]
      [else
       (define-values (s e) (send t get-token-range pos))
       (define category (classify-position t s))
       (case category
         [(whitespace comment continue-operator)
          (if (zero? s)
              (or candidate 0)
              (loop (sub1 s) candidate at-start))]
         [(opener)
          candidate]
         [(closer)
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
             (loop (sub1 r) (col-of r start delta) start)])]
         [(block-operator bar-operator separator) candidate]
         [else
          (cond
            [(and #f ;; not sure why special-casing operators seemed like a good idea
                  for-outdent?
                  (eq? category 'operator))
             (if (zero? s)
                 (or candidate 0)
                 (loop (sub1 s) candidate at-start))]
            [(zero? s) 0]
            [else
             (define start (line-start t pos))
             (define delta (line-delta t start))
             (loop (sub1 s) (col-of s start delta) start)])])])))

(define (whitespace? str)
  (and (= (string-length str) 1)
       (char-whitespace? (string-ref str 0))
       (not (equal? str "\n"))))

;; determine current indentation starting with `start`
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

;; expects s to be at the start of non-whitespace; gets the end
;; of the current expression, which might be the end of a block
;; that starts at `s`
(define (end-of-current t s-in #:stop-at-comma? [stop-at-comma? #f])
  (define-values (s e) (send t get-token-range s-in))
  (define category (classify-position t s))
  (case category
    [(#f) s]
    [(opener)
     (send t forward-match s (send t last-position))]
    [(closer) s]
    [(block-operator bar-operator)
     (define-values (next-s next-e) (skip-whitespace t e 1))
     (define start (line-start t next-s))
     (define delta (line-delta t start))
     (skip-to-shallower t e (col-of next-s start delta)
                        #:bar-stop-line (and (eq? category 'bar-operator)
                                             start))]
    [(comma-operator) (if stop-at-comma? s e)]
    [else e]))

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
       (define category (classify-position t s))
       (case category
         [(whitespace comment continue-operator)
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
               [(opener)
                (define o-e (send t forward-match s end-pos))
                (if o-e
                    (loop o-e o-e)
                    s)]
               [(closer) s]
               [else (loop e e)])])])])))

;; return the start of the group containing `pos`, but if `pos`
;; is already the start of the group and `or-out?`, return the start of the
;; enclosing group
(define (start-of-group t orig-pos at-start
                        #:or-out? [or-out? #f])
  (define (finish last-pos)
    (or last-pos
        (if or-out?
            (start-of-enclosing-block t orig-pos)
            orig-pos)))
  (let loop ([pos orig-pos] [last-pos #f] [at-start at-start])
    (cond
      [(= pos 0) (or last-pos 0)]
      [else
       (define-values (s e) (send t get-token-range (sub1 pos)))
       (define category (classify-position t s))
       (case category
         [(whitespace comment)
          (define start (line-start t s))
          (if (eqv? start at-start)
              (loop s last-pos at-start)
              (finish last-pos))]
         [(continue-operator)
          (loop s last-pos (line-start t s))]
         [(block-operator)
          (or last-pos (if or-out? s orig-pos))]
         [(bar-operator)
          (or last-pos (if or-out? s orig-pos))]
         [(opener closer)
          (define o-s (send t backward-match e 0))
          (cond
            [o-s ; => `s` is a closer
             (define start (line-start t e))
             (if (eqv? start at-start)
                 (loop o-s o-s (line-start t o-s))
                 (finish last-pos))]
            [else ; `s` is an opener or unmatched closer
             (or last-pos (if or-out? s orig-pos))])]
         [(comma-operator)
          (finish last-pos)]
         [else
          (define start (line-start t s))
          (if (eqv? start at-start)
              (loop s s at-start)
              (finish last-pos))])])))

;; return the start of the block or parens containing the group containg `pos`
(define (start-of-enclosing-block t pos)
  (define start (line-start t pos))
  (define col (col-of pos start (line-delta t start)))
  (cond
    [(zero? col)
     ;; use column 0 as a proxy for being in a top-level block; we
     ;; don't really want to go up (to the start of the buffer)
     ;; from there
     pos]
    [else
     (let loop ([pos pos] [last-pos #f])
       (cond
         [(= pos 0) 0]
         [else
          (define-values (s e) (send t get-token-range (sub1 pos)))
          (define category (classify-position t s))
          (case category
            [(whitespace comment continue-operator) (loop s last-pos)]
            [(closer)
             (define o-s (send t backward-match e 0))
             (cond
               [o-s
                (loop o-s o-s)]
               [else ; unmatched
                (loop s s)])]
            [(opener) s]
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
             (loop s s)])]))]))



(define (skip-whitespace t pos dir
                         #:and-separators? [and-separators? #f]
                         #:stay-on-line [stay-on-line #f])
  (define end-pos (send t last-position))
  (cond
    [(= pos -1) (send t get-token-range 0)]
    [(pos . >= . end-pos) (send t get-token-range (sub1 end-pos))]
    [else
     (let loop ([pos pos] [stay-on-line stay-on-line])
       (define-values (s e) (send t get-token-range pos))
       (define category (classify-position t s))
       (define (continue #:ok-to-change-line? [ok-to-change-line? #f])
         (define start (and stay-on-line (line-start t (if (positive? dir) e s))))
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
       (case category
         [(whitespace comment)
          (continue)]
         [(continue-operator)
          (continue #:ok-to-change-line? #t)]
         [(separator)
          (if and-separators?
              (continue)
              (values s e))]
         [else (values s e)]))]))

(define (skip-hash-lang t pos)
  (define category (send t classify-position pos)) ; not `classify-position*`
  (case category
    [(other)
     ;; keep skiping past non-comment whitespace
     (define-values (s e) (send t get-token-range pos))
     (let loop ([pos e])
       (case (classify-position t pos)
         [(whitespace)
          (define-values (s e) (send t get-token-range pos))
          (loop e)]
         [else pos]))]
    [else pos]))
