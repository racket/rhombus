#lang racket/base
(require racket/class
         "../lex.rkt")

(provide like-text%)

(define TOKEN-SLOT 0)
(define TYPE-SLOT 1)

;; fix 1-based indexing...
(define (srcloc-0position loc)
  (sub1 (srcloc-position loc)))

(define like-text%
  (class object%
    (init-field content
                [lex-all-input lex-all])

    (super-new)

    (define-values (position-paragraphs paragraph-starts)
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

    (define/public (get-token-range pos)
      (define t+type (or (hash-ref mapping pos #f)
                         (error 'get-token-range "lookup failed: ~e" pos)))
      (define t (vector-ref t+type TOKEN-SLOT))
      (define loc (token-srcloc t))
      (values (srcloc-0position loc)
              (+ (srcloc-0position loc) (srcloc-span loc))))

    (define/public (last-position)
      (string-length content))

    (define/public (position-paragraph pos [eol? #f])
      (or (hash-ref position-paragraphs pos #f)
          (error 'position-paragraph "lookup failed: ~e" pos)))
    
    (define/public (paragraph-start-position para)
      (or (hash-ref paragraph-starts para #f)
          (error 'paragraph-start-position "lookup failed: ~e" para)))

    (define/public (backward-match pos cutoff)
      (let loop ([pos (sub1 pos)] [depth -1] [need-close? #t])
        (cond
          [(pos . < . 0) #f]
          [else
           (define-values (s e) (get-token-range pos))
           (define category (classify-position pos))
           (case category
             [(parenthesis)
              (case (get-text s e)
                [("{" "(" "[" "«") (and (not need-close?)
                                        (if (= depth 0)
                                            s
                                            (loop (sub1 s) (sub1 depth) #f)))]
                [("}" ")" "]" "»") (loop (sub1 s) (add1 depth) #f)]
                [else (error "unexpected parenthesis-class text")])]
             [(whitespace comment)
              (loop (sub1 s) depth need-close?)]
             [else (if need-close?
                       s
                       (loop (sub1 s) depth #f))])])))

    (define/public (forward-match pos cutoff)
      (let loop ([pos pos] [depth 0])
        (define-values (s e) (get-token-range pos))
        (cond
          [(not s) #f]
          [else
           (define category (classify-position pos))
           (case category
             [(parenthesis)
              (case (get-text s e)
                [("{" "(" "[" "«")
                 (loop e (add1 depth))]
                [("}" ")" "]" "»")
                 (if (depth . <= . 1)
                     e
                     (loop e (sub1 depth)))]
                [else (error "unexpected parenthesis-class text")])]
             [else
              (loop e depth)])])))))
