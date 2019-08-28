#lang racket/base
(require racket/match
         racket/list
         racket/set
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax)
         racket/generic)

(struct loc (line col pos) #:transparent)
(define (current-loc ip)
  (call-with-values (λ () (port-next-location ip)) loc))

(define (string->set s) (list->set (string->list s)))
(define ((set-mem s) v) (set-member? s v))

(define indent-amount 2)
(define line-space-follower (string->set "|:@\\&"))
(define line-follower (set-union line-space-follower (string->set "]\n") (set eof)))
(define follower (set-union line-follower (string->set " .,'()[]<>{}") (set eof)))
(define number-follower (set-remove follower #\.))
(define number-leader (string->set "-+0123456789"))
(define text-follower (set-union (string->set "@{}\n") (set eof)))

(define (operator? x)
  (and (symbol? x)
       (regexp-match #px"^\\p{^L}*$" (symbol->string x))))
(define PRECEDENCE-ORDER
  '(* / % + - #t < <= == != >= >))

(define-values (precedence-table default-precedence)
  (for/fold ([pt (hasheq)] [dp #f])
            ([o (in-list PRECEDENCE-ORDER)]
             [i (in-naturals)])
    (cond
      [(eq? o #t)
       (values pt i)]
      [else
       (values (hash-set pt o i) dp)])))

(define (#%dot-list x y)
  (list* '#%dot x
         (match y
           [(cons '#%dot y) y]
           [_ (list y)])))

;; XXX srclocs / syntax

(define (read-lexpr ip)
  (port-count-lines! ip)

  (define (parse-error . state)
    (error 'parse-error "~v => unexpected ~v: ~e" state (peek-char ip) (read-bytes 128 ip)))
  (define (spy . state)
    (when #f
      (eprintf "spy: ~v: ~e\n"
               state
               (peek-bytes 128 0 ip))))

  (define (reads-until s)
    (define cs
      (let loop ([cs 0] [off 0])
        (define c (peek-char ip off))
        (if (set-member? s c)
          cs
          (loop (add1 cs) (+ off (char-utf-8-length c))))))
    (read-string cs ip))
  (define (expectc x . state)
    (define y (peek-char ip))
    (if (equal? x y)
      (read-char ip)
      (parse-error 'expectc state x)))

  (define (infixate in)
    (sy:consume-input in '() '()))
  (define (sy:precendence op)
    (hash-ref precedence-table op default-precedence))
  (define (sy:consume-input in out ops)
    #;(eprintf "SY in ~v\n" (vector in out ops))
    (match in
      ['()
       (sy:pop-operators out ops)]
      [(cons token in)
       (cond
         [(operator? token)
          (define-values (out-p ops-p)
            (sy:push-operator out ops token))
          (sy:consume-input in out-p ops-p)]
         [else
          (sy:consume-input in (cons token out) ops)])]))
  (define (sy:push-operator out ops op1)
    #;(eprintf "SY push ~v\n" (vector out ops op1))
    (match ops
      ['()
       (values out (cons op1 ops))]
      [(cons op2 ops-p)
       (cond
         [(<= (sy:precendence op2) (sy:precendence op1))
          (sy:push-operator
           (sy:push-operator-to-output op2 out)
           ops-p op1)]
         [else
          (values out (cons op1 ops))])]))
  (define (sy:pop-operators out ops)
    #;(eprintf "SY pop ~v\n" (vector out ops))
    (match ops
      ['()
       (match out
         [(list result)
          result]
         [_
          (error 'sy:pop-operators "Too much output: ~v" out)])]
      [(cons op ops)
       (sy:pop-operators
        (sy:push-operator-to-output op out)
        ops)]))
  (define (sy:push-operator-to-output op out)
    #;(eprintf "SY push-out ~v\n" (vector op out))
    (match out
      [(list* arg2 arg1 out)
       (cons (list op arg1 arg2)
             out)]))

  (define (text-mode p)
    (cons '#%text (text-mode* p 0)))
  (define (text-mode* p braces)
    (define s (reads-until text-follower))
    (match (peek-char ip)
      [(? eof-object?) (parse-error 'text-mode p braces)]
      [#\newline (read-char ip)
       (cons (text-single s)
             (if (expect-prefix p)
               (text-mode* p braces)
               '()))]
      [#\{ (read-char ip)
       (text-cons (text-cons1 s "{")
                  (text-mode* p (add1 braces)))]
      [#\} (read-char ip)
       (if (zero? braces)
         (text-cons (text-single s)
                    '())
         (text-cons (text-cons1 s "}")
                    (text-mode* p (sub1 braces))))]
      [#\@ (read-char ip)
       (text-cons (text-cons1 s (list '#%text-esc (unit)))
                  (text-mode* p braces))]))
  (define (text-cons pre post)
    (match post
      ['() (list pre)]
      [(cons x y)
       (cons (append pre x) y)]))
  (define (text-cons1 x y)
    (append (text-single x)
            (text-single y)))
  (define (text-single x)
    (if (equal? "" x) '() (list x)))

  (define (leader)
    (match (peek-char ip)
      [(? (set-mem number-leader))
       (define s (reads-until number-follower))
       (define n (string->number s))
       (if n n (string->symbol s))]
      [#\' (read-char ip)
       (list '#%quote (unit))]
      [#\, (read-char ip)
       (list '#%unquote (unit))]
      [#\# (read-char ip)
       (expectc #\\)
       (read-char ip)]
      [#\( (read-char ip)
       (infixate (grouped #\)))]
      [#\{ (read-char ip)
       (text-mode #f)]
      [#\[ (read-char ip)
       (begin0 (list '#%quote-line
                     (parameterize ([current-line-quoted? #t])
                       (line #f)))
         (expectc #\]))]
      [#\< (read-char ip)
       (if (set-member? follower (peek-char ip))
         '<
         (grouped #\>))]
      [#\> (read-char ip)  
       '>]
      [x
       (define l (reads-until follower))
       (when (equal? "" l)
         (parse-error 'leader))
       (string->symbol l)]))

  (define (after-leader l)
    (match (peek-char ip)
      [#\. (read-char ip)
       (#%dot-list l (unit))]
      [#\( (read-char ip)
       (after-leader (list* '#%fun-app l (seq #\))))]
      [#\[ (read-char ip)
       (after-leader (list* '#%member l (seq #\])))]
      [#\< (read-char ip)
       (after-leader (list* '#%param l (seq #\>)))]
      [#\{ (read-char ip)
       (after-leader (list* '#%text-app l (rest (text-mode #f))))]
      [_ l]))

  (define (unit)
    (after-leader (leader)))

  (define (seq1 endc)
    (infixate (units (set endc #\,))))
  (define (seq endc)
    (match (peek-char ip)
      [(== endc) (read-char ip)
                 '()]
      [_
       (cons (seq1 endc) (seq-tail endc))]))
  (define (seq-tail endc)
    (match (peek-char ip)
      [(== endc) (read-char ip)
                 '()]
      [#\, (read-char ip)
       (expectc #\space)
       (cons (seq1 endc) (seq-tail endc))]
      [_
       (parse-error 'seq-tail endc)]))

  (define (grouped endc)
    (begin0 (units (set endc))
      (expectc endc)))

  (define (units stops)
    (match (peek-char ip)
      [(? (set-mem stops))
       '()]
      [_
       (cons (unit) (units-tail stops))]))
  (define (units-tail stops)
    (match (peek-char ip)
      [(? (set-mem stops))
       '()]
      [#\space (read-char ip)
       (cons (unit) (units-tail stops))]))

  (struct prefix:left-col (lc) #:transparent)
  (struct prefix:first-bar (bc lc) #:transparent)
  (struct prefix:bar (bc) #:transparent)
  (define (prefix-indent p)
    (match p
      [(prefix:left-col lc)
       (prefix:left-col (+ indent-amount lc))]
      [_
       (error 'prefix-indent "~v" p)]))
  (define (prefix-next/line p)
    (match p
      [#f #f]
      [(prefix:left-col _) p]
      [(prefix:first-bar _ lc)
       (prefix:left-col lc)]
      [(prefix:bar bc)
       (prefix:left-col (+ 2 bc))]
      [_
       (error 'prefix-next/line "~v" p)]))
  (define (prefix-next/lines p)
    (match p
      [#f #f]
      [(prefix:left-col _) p]
      [(prefix:first-bar bc lc)
       (prefix:bar bc)]
      [(prefix:bar bc)
       (prefix:bar bc)]
      [_
       (error 'prefix-next/lines "~v" p)]))

  (define (expect-prefix p)
    (spy 'expect-prefix p)
    (match p
      [#f #t]
      [(prefix:left-col n)
       (and (for/and ([i (in-range n)])
              (equal? #\space (peek-char ip i)))
            (read-string n ip))]
      [(prefix:bar bc)
       (and (for/and ([i (in-range bc)])
              (equal? #\space (peek-char ip i)))
            (equal? #\| (peek-char ip bc))
            (equal? #\space (peek-char ip (add1 bc)))
            (read-string (+ 2 bc) ip))]
      [(prefix:first-bar _ _)
       #t]
      [_
       (error 'expect-prefix "~v" p)]))

  (define current-line-quoted? (make-parameter #f))
  (define (line *p)
    (spy 'line *p)
    (define p (or *p (prefix:left-col (loc-col (current-loc ip)))))
    (line-start p))
  (define (line-start p)
    (spy 'line-start p)
    (match (peek-char ip)
      [(? (set-mem line-space-follower)) (line-space-tail p)]
      [(? (set-mem line-follower)) (line-tail p)]
      [_ (cons (unit) (line-tail p))]))
  (define (line-tail p)
    (spy 'line-tail p)
    (match (peek-char ip)
      [(? eof-object?) '()]
      [#\newline (read-char ip)
       '()]
      [#\space (read-char ip)
       (line-space-tail p)]
      [#\]
       (cond
         [(current-line-quoted?)
          '()]
         [else
          (parse-error 'line-tail p)])]
      [_
       (parse-error 'line p)]))
  (define (line-space-tail p)
    (spy 'line-space-tail p)
    (match (peek-char ip)
      [#\\ (read-char ip)
       (expectc #\newline)
       (define new-p (prefix-indent p))
       (if (expect-prefix new-p)
         (line-start new-p)
         '())]
      [#\& (read-char ip)
       (expectc #\newline)
       (if (expect-prefix p)
         (line-start p)
         '())]
      [#\: (read-char ip)
       (expectc #\newline)
       (define new-p (prefix-indent p))
       (cons
        (cons '#%indent (lines new-p))
        (if (expect-prefix p)
          (line-start p)
          '()))]
      [#\@ (read-char ip)
       (expectc #\newline)
       (define new-p (prefix-indent p))
       (append
        (if (expect-prefix new-p)
          (list (text-mode new-p))
          '())
        (line-start p))]
      [#\| (read-char ip)
       (expectc #\space)
       (define this-loc (loc-col (current-loc ip)))
       (define new-p (prefix:first-bar (- this-loc 2) this-loc))
       (cons
        (cons '#%bar (lines new-p))
        (line-start p))]
      [_
       (cons (unit) (line-tail p))]))

  (define (lines p)
    (spy 'lines p)
    (cond
      [(expect-prefix p)
       (match (peek-char ip)
         [(? eof-object?) '()]
         [#\newline (read-char ip)
          (lines (prefix-next/lines p))]
         [_
          (cons (line (prefix-next/line p))
                (lines (prefix-next/lines p)))])]
      [else
       '()]))

  (lines #f))

(module+ test
  (require racket/list
           racket/file
           racket/string
           racket/port
           racket/system
           racket/pretty
           racket/runtime-path
           syntax/location)
  (define (value->file f v)
    (with-output-to-file f (λ () (pretty-write v)) #:exists 'replace))

  (define-runtime-path actual.rktd "actual.rktd")
  (define-runtime-path expected.rktd "expected.rktd")
  (define (read-test label in expected)
    #;(eprintf "Reading ~a\n" label)
    (define actual
      (with-handlers
          ([exn:fail?
            (λ (x)
              (define xp
                (struct-copy exn x
                             [message (format "Reading ~a: ~a" label (exn-message x))]))
              (raise xp))])
        (call-with-input-string in read-lexpr)))
    (cond
      [(equal? actual expected)
       #;(eprintf "...test passed\n")]
      [else
       (eprintf "Test failed: ~a: \n" label)
       (displayln in)
       (value->file actual.rktd actual)
       (value->file expected.rktd expected)
       (system* (find-executable-path "diff") "-u"
                actual.rktd expected.rktd)
       (displayln "")]))

  (define-simple-macro (rt in e)
    (read-test (quote-srcloc) in 'e))

  (define-runtime-path x-in "x.txt")
  (define-runtime-path x-out "x.rktd")
  #;(parse-test "x" (file->string x-in) (file->value x-out))

  (define (extract-md-block lang line l)
    (define-values (ignored block-start)
      (splitf-at l (λ (s) (not (string=? s lang)))))
    (define bs-line (+ line 2 (length ignored)))
    (cond
      [(empty? block-start)
       (values line "" bs-line '())]
      [else
       (define-values (block after-block)
         (splitf-at (rest block-start) (λ (s) (not (string=? s "```")))))
       (if (empty? after-block)
         (error 'extract-md-block "Block did not end")
         (values bs-line (string-join block "\n") (+ bs-line (length block)) (rest after-block)))]))
  (define (extract-tests line l)
    (define-values (le-line lexpr more-line more) (extract-md-block "```lexpr" line l))
    (define-values (se-line sexpr after-line after) (extract-md-block "```sexpr" more-line more))
    (unless (and (string=? "" lexpr) (string=? "" sexpr))
      (read-test (format "extracted L~a" le-line) lexpr (with-input-from-string sexpr read))
      (extract-tests after-line after)))

  (define-runtime-path md "../0004-lexpr.md")
  (extract-tests 0 (file->lines md)))
