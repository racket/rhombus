#lang racket/base
(require racket/class)

(provide opener?
         line-start
         line-delta
         col-of
         only-whitespace-between?
         get-block-column)

(define (opener? s)
  (member s '("(" "{" "[")))

(define (line-start t pos)
  (send t paragraph-start-position (send t position-paragraph pos #t)))

(define (line-delta t start #:unless-empty? [unless-empty? #f])
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
    (and (case (send t classify-position pos)
           [(white-space) #t]
           [(comment continue-operator) or-ws-like?]
           [else #f])
         (let ()
           (define-values (s e) (send t get-token-range pos))
           (or (e . >= . e-pos)
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
       (define category (send t classify-position s))
       (case category
         [(white-space comment continue-operator)
          (if (zero? s)
              (or candidate 0)
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
            [(and for-outdent?
                  (eq? category 'operator))
             (if (zero? s)
                 (or candidate 0)
                 (loop (sub1 s) candidate at-start))]
            [(zero? s) 0]
            [else
             (define start (line-start t pos))
             (define delta (line-delta t start))
             (loop (sub1 s) (col-of s start delta) start)])])])))
