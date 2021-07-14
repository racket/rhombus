#lang at-exp racket/base
(require racket/class
         "../lex.rkt"
         "../indentation.rkt"
         (for-syntax racket/base))

(define failed? #f)

(define TOKEN-SLOT 0)
(define TYPE-SLOT 1)

;; fix 1-based indexing...
(define (srcloc-0position loc)
  (sub1 (srcloc-position loc)))

(define like-text%
  (class object%
    (init-field content)

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
      (lex-all (let ([p (open-input-string content)])
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

    (define/public (classify-position pos)
      (define t+type (or (hash-ref mapping pos #f)
                         (error 'classify-position "lookup failed: ~e" pos)))
      (vector-ref t+type TYPE-SLOT))

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
                [("{" "(" "[") (and (not need-close?)
                                    (if (= depth 0)
                                        s
                                        (loop (sub1 s) (sub1 depth) #f)))]
                [("}" ")" "]") (loop (sub1 s) (add1 depth) #f)]
                [else (error "unexpected parenthesis-class text")])]
             [(whitespace comment)
              (loop (sub1 s) depth need-close?)]
             [else (if need-close?
                       s
                       (loop (sub1 s) depth #f))])])))))
    

(define (check e line)
  (printf "Checking ~s\n" line)
  ;; replacing each "^" by two spaces pushes the token too
  ;; far to the right, which makes the `#:multi?` result
  ;; keep its original sorting right-to-left:
  (define clean-e (regexp-replace* #rx"\\!" (regexp-replace* #rx"\\^" e "  ") "x"))
  (define m (regexp-match-positions #rx"[ ]*(?:\\^|[!])" e))
  (define start-pos (caar m))
  (define expected (reverse
                    (let loop ([pos start-pos])
                      (cond
                        [(= pos (string-length e)) '()]
                        [(memv (string-ref e pos) '(#\^ #\!))
                         (cons (- pos start-pos) (loop (add1 pos)))]
                        [else (loop (add1 pos))]))))
  (define t (new like-text% [content clean-e]))
  (define raw-candidates (shrubbery-indentation t start-pos #:multi? #t))
  (define candidates (if (list? raw-candidates) raw-candidates (list raw-candidates)))
  (unless (equal? candidates expected)
    (set! failed? #t)
    (eprintf (string-append
              "failed at line ~a\n"
              "  expected: ~a\n"
              "  received: ~a\n")
             line
             expected
             candidates)))

(define-syntax-rule (check-all e ...)
  (begin
    (check e (at e))
    ...))

(define-syntax (at stx)
  (syntax-case stx ()
    [(_ e) #`#,(syntax-line #'e)]))

(define e string-append)

(check-all

 ;; In these tests, the last line of each is the one to be indented,
 ;; and the "^"s (which will be replaced by two spaces before applying
 ;; the indenter) or "!"s (replaced by an "x") are the expected
 ;; indentation candidates. The content after the last "^" or "!", if
 ;; any, can affect the indenters results (e.g., the answer tends to
 ;; be different if it starts "|"). Whitespace after a final "^" or
 ;; "!" will not matter.
 
 @e{a
    ^x}

 @e{a:
    ^ ^x}

 @e{apple:
    ^ ^x}

 @e{apple: banana:
    ^      ^ ^x}

 @e{apple: :
    ^        ^x}

 @e{(a:
     ^ ^x}

 @e{(:
       ^x}

 @e{[a:
     ^ ^x}

 @e{[:
       ^x}

 @e|{{a:
      ^ ^x}|

 @e|{{:
      ^ ^x}|

 @e{define pi:
    ^ ^x}

 @e{define pi: 3.14
    ^          ^x}

 @e{define fib(n):
     cond | n == 0: 0
          ^          ^|}

 @e{define fib(n):
     log_error("fib called")
     cond | n == 0: 0
          ^          ^|}
 
 @e{define
     | fib(0): 0
     | fib(1): 1
     | fib(n): (fib(n-1) // in parens => `+` continues this line
                ^+ fib(n-2))}

 @e{define fib:
     lambda (n):
       cond
        | n == 0: 0
        ^          ^| n == 1: 1}

 @e|{define fib(n):
      match n { | 0 { 0 }
                ^| 1 { 1 }}|

 @e{define analyze(n):
      if n == 0
       | printf("zero\n")
    ^ ^  ^printf}

 @e{define go():
      define more(m):
        if m == 0 | "done"
                  ^|}

 @e{define go():
      define more(m):
        if m == 0 | "done"
    ^ ^ ^           ^x}


 @e{define approx_thunk(x):
      match x
       | something(v): lambda
                        | (): v
       ^                ^      ^| (n): v+n}

 @e{define colors:
     list(
       ^red,}

 @e{define colors:
     list(
       red,
       ^green,}

 @e{define colors:
     list(
       red
     ^)}

 @e{define colors:
     list(red,
          ^green,}

 @e{this is a \
      !ong linear group}

 @e{this is a \
      very long linear group \
      !hat spans multiple lines}

 @e{this is | one: a \
                    long result
            ^       ^| two}

 @e{this is | one: a \
                    long result
            | two \
                !lso long}

 @e{this is a group \  with (a,
                             ^nested}

 @e{this is a group \
      with (a,
                             ^nested}

 @e{this is a group \
     with (a,
                    \
           ^nested}

 @e{hello | a | c\
     :
    ^           ^    ^d}

 @e{nonsense:
      hello | there 4.5
            | more f(8)
              next (ok |
                       ^|}

 @e{nonsense:
      hello | there 4.5
            | more f(8)
              next (ok |
                       | stuff: (begin:
                                   more
                                 ^ ^things}

 @e{x + w | z : :
    ^       ^     ^y}

 @e{x something | a
                  y:
                    w:
    ^             ^ ^ ^q}

 @e{define fib(n):
      match n
       | 0
    ^ ^  ^: 0}

 @e{+ `(
         ^x}

 @e{+ `(
         x
       ^)}
 
 @e{+ x`(
        ^x}

 @e{+ x`(
        x
      ^)}
 
 )

(when failed?
  (error "failed"))
