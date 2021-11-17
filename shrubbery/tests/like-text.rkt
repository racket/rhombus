#lang racket/base
(require racket/class
         "../lex.rkt"
         "../private/paren.rkt")

(provide like-text%)

(define TOKEN-SLOT 0)
(define TYPE-SLOT 1)
(define PAREN-SLOT 2)

;; fix 1-based indexing...
(define (srcloc-0position loc)
  (sub1 (srcloc-position loc)))

(define like-text%
  (class object%
    (init-field content
                [lex-all-input lex-all])

    (super-new)

    (define/private (find-paragraphs)
      (let loop ([pos 0] [para 0] [pos-para #hasheqv()] [para-pos #hasheqv((0 . 0))])
        (cond
          [(= pos (string-length content))
           (values (hash-set pos-para pos para) para-pos)]
          [(char=? #\newline (string-ref content pos))
           (loop (add1 pos) (add1 para)
                 (hash-set pos-para pos para)
                 (hash-set para-pos (add1 para) (add1 pos)))]
          [else
           (loop (add1 pos) para (hash-set pos-para pos para) para-pos)])))

    (define-values (position-paragraphs paragraph-starts)
      (find-paragraphs))

    (define tokens
      (lex-all-input (let ([p (open-input-string content)])
                       (port-count-lines! p)
                       p)
                     (lambda args
                       (error "unexpected lex failure: ~s" args))
                     #:keep-type? #t))

    ;; position -> token
    (define mapping
      (let loop ([tokens tokens] [pos 0] [mapping #hasheqv()])
        (cond
          [(null? tokens) mapping]
          [else
           (define t+type (car tokens))
           (define t (vector-ref t+type TOKEN-SLOT))
           (define loc (token-srcloc t))
           (unless (= pos (srcloc-0position loc))
             (error 'test "token discontinuity ~s vs. ~v @ ~s" pos (token-e t) (srcloc-0position loc)))
           (loop (cdr tokens)
                 (+ pos (srcloc-span loc))
                 (for/fold ([mapping mapping]) ([i (in-range (srcloc-span loc))])
                   (hash-set mapping (+ pos i) t+type)))])))

    (define/public (get-text s e)
      (substring content s e))

    (define/public (classify-position* pos)
      (define t+type (or (hash-ref mapping pos #f)
                         (error 'classify-position "lookup failed: ~e" pos)))
      (vector-ref t+type TYPE-SLOT))

    (define/public (classify-position pos)
      (define attribs (classify-position* pos))
      (if (symbol? attribs)
          attribs
          (hash-ref attribs 'type 'unknown)))

    (define/private (get-paren pos)
      (define t+type (or (hash-ref mapping pos #f)
                         (error 'get-paren "lookup failed: ~e" pos)))
      (vector-ref t+type PAREN-SLOT))

    (define/public (get-token-range pos)
      (define t+type (or (hash-ref mapping pos #f)
                         (error 'get-token-range "lookup failed: ~e" pos)))
      (define t (vector-ref t+type TOKEN-SLOT))
      (define loc (token-srcloc t))
      (values (srcloc-0position loc)
              (+ (srcloc-0position loc) (srcloc-span loc))))

    (define/public (last-position)
      (string-length content))

    (define/public (set-position start [end start])
      (void))
    (define/public (get-end-position)
      (last-position))

    (define/public (position-paragraph pos [eol? #f])
      (or (hash-ref position-paragraphs pos #f)
          (error 'position-paragraph "lookup failed: ~e" pos)))
    
    (define/public (paragraph-start-position para)
      (or (hash-ref paragraph-starts para #f)
          (error 'paragraph-start-position "lookup failed: ~e" para)))

    (define/public (paragraph-end-position para)
      (or (hash-ref paragraph-starts (add1 para) #f)
          (last-position)))

    (define/public (begin-edit-sequence . args) (void))
    (define/public (end-edit-sequence . args) (void))

    (define/public (insert str pos [end pos])
      (set! content (string-append
                     (substring content 0 pos)
                     str
                     (substring content end)))
      (set!-values (position-paragraphs paragraph-starts) (find-paragraphs)))

    (define/public (backward-match pos cutoff)
      (backward-matching-search pos cutoff 'one))

    (define/public (backward-containing-sexp pos cutoff)
      (backward-matching-search pos cutoff 'all))

    (define/private (backward-matching-search init-pos cutoff mode)
      (define start-pos (if (and (eq? mode 'all)
                                 (init-pos . <= . cutoff))
                            cutoff
                            (sub1 init-pos)))
      (let loop ([pos start-pos] [depth (if (eq? mode 'one) -1 0)] [need-close? (eq? mode 'one)])
        (cond
          [(pos . < . cutoff) #f]
          [else
           (define-values (s e) (get-token-range pos))
           (define (atom)
             (if need-close?
                 s
                 (loop (sub1 s) depth #f)))
           (define sym (get-paren s))
           (cond
             [sym
              (let paren-loop ([parens shrubbery-paren-matches])
                (cond
                  [(null? parens)
                   ;; treat an unrecognized parenthesis like an atom
                   (atom)]
                  [(eq? sym (caar parens))
                   (and (not need-close?)
                        (if (= depth 0)
                            (cond
                              [(eq? mode 'all)
                               ;; color:text% method skips back over whitespace, but
                               ;; doesn't go beyond the starting position
                               (min (skip-whitespace e 'forward #f)
                                    init-pos)]
                              [else s])
                            (loop (sub1 s) (sub1 depth) #f)))]
                  [(eq? sym (cadar parens))
                   (cond
                     [(e . > . init-pos)
                      ;; started in middle of closer
                      (if (eq? mode 'one)
                          s
                          (loop (sub1 s) depth #f))]
                     [else (loop (sub1 s) (add1 depth) #f)])]
                  [else
                   (paren-loop (cdr parens))]))]
             [else
              (define category (classify-position pos))
              (case category
                [(white-space comment)
                 (loop (sub1 s) depth need-close?)]
                [else (atom)])])])))

    (define/public (forward-match pos cutoff)
      (let loop ([pos pos] [depth 0])
        (define-values (s e) (get-token-range pos))
        (cond
          [(not s) #f]
          [else
           (define sym (get-paren s))
           (define (atom)
             (if (zero? depth)
                 e ;; didn't find paren to match, so finding token end
                 (loop e depth)))
           (cond
             [sym
              (let paren-loop ([parens shrubbery-paren-matches])
                (cond
                  [(null? parens)
                   ;; treat an unrecognized parenthesis like an atom
                   (atom)]
                  [(eq? sym (caar parens))
                   (if (eqv? pos s) ; don't count the middle of a parenthesis token
                       (loop e (add1 depth))
                       e)]
                  [(eq? sym (cadar parens))
                   (cond
                     [(depth . <= . 0) #f]
                     [(depth . = . 1) e]
                     [else (loop e (sub1 depth))])]
                  [else
                   (paren-loop (cdr parens))]))]
             [else
              (define category (classify-position pos))
              (case category
                [(white-space comment) (loop e depth)]
                [else (atom)])])])))

    (define/public (skip-whitespace pos dir comments?)
      (define (skip? category)
        (or (eq? category 'white-space)
            (and comments? (eq? category 'comment))))
      (case dir
        [(forward)
         (let loop ([pos pos])
           (define category (classify-position pos))
           (cond
             [(skip? category)
              (define-values (s e) (get-token-range pos))
              (if e
                  (loop e)
                  pos)]
             [else pos]))]
        [(backward)
         (cond
           [(zero? pos) 0]
           [else
            (let loop ([pos (sub1 pos)] [end-pos pos])
              (define category (classify-position pos))
              (cond
                [(skip? category)
                 (define-values (s e) (get-token-range pos))
                 (loop (sub1 s) s)]
                [else end-pos]))])]
        [else
         (error 'skip-whitespace "bad direction: ~e" dir)]))))
