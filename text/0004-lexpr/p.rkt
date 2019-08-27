#lang racket/base
(require racket/match
         racket/list
         srfi/14)

(struct loc (line col pos) #:transparent)
(define (current-loc ip)
  (call-with-values (λ () (port-next-location ip)) loc))

(define (parse ip)
  (port-count-lines! ip)

  (define (peek? cs)
    (define c (peek-char ip))
    (and (char? c)
         (char-set-contains? cs c)))
  (define (check1 c)
    (and (equal? c (peek-char ip))
         (read-char ip)))
  (define (expect1 c state . params)
    (unless (check1 c)
      (parse-error state params)))
  (define (consume cs)
    (and (peek? cs)
         (cons (read-char ip)
               (or (consume cs) '()))))
  (define (consume-re r)
    (regexp-try-match r ip))

  (define (parse-error state . params)
    (error 'parse-error "~a: ~v => unexpected ~v: ~e" state params (peek-char ip) (read-bytes 128 ip)))

  (define empty-cs (string->char-set ""))
  (define space-cs (string->char-set " "))
  (define identifier-cs
    (char-set-union char-set:letter char-set:digit
                    ;; XXX ugh
                    (string->char-set "_<>+-*/")))

  (define (parse-prefix pre)
    (match pre
      ['()
       (not (eof-object? (peek-char ip)))]
      [(cons 'first-bar _)
       (check1 #\space)]
      [(cons (? number? n) _)
       (define s (peek-string (add1 n) 0 ip))
       (and (string? s) (regexp-match #rx"^ +| $" s)
            (read-string (add1 n) ip))]
      [(cons ': pre)
       (and (check1 #\space) (check1 #\space)
            (parse-prefix pre))]))

  (define (parse-whitespace)
    (cond
      [(consume space-cs)
       #t]
      ;; XXX comments
      [else
       #f]))

  (define (parse-iexpr stop-cs)
    (cond
      [(check1 #\")
       (begin0 (list->string (consume (char-set-complement (string->char-set "\""))))
         (expect1 #\" 'iexpr))]
      [(consume-re #rx"^[0-9]+(\\.[0-9]+)?")
       => (λ (m) (string->number m))]
      [(consume (char-set-difference identifier-cs stop-cs))
       => (λ (m) (string->symbol (list->string m)))]
      [else
       (parse-error 'iexpr (char-set->string stop-cs))]))

  (define (parse-qexpr stop-cs)
    (cond
      [(peek? stop-cs)
       '()]
      [else
       (cons (parse-iexpr stop-cs) (parse-q*expr stop-cs))]))

  (define (parse-q*expr stop-cs)
    (if (parse-whitespace)
      (parse-qexpr stop-cs)
      '()))

  (define (parse-ltail pre)
    (cond
      [(or (check1 #\newline)
           (check1 eof)) '()]
      [(check1 #\:)
       (consume space-cs)
       (expect1 #\newline 'ltail ': pre)
       (cons (cons ': (parse-lexprs (cons ': pre)))
             (or (parse-lexpr pre #t)
                 '()))]
      [(check1 #\&)
       (consume space-cs)
       (expect1 #\newline 'ltail '& pre)
       (parse-lexprs pre)]
      [(check1 #\\)
       (consume space-cs)
       (expect1 #\newline 'ltail '|\\| pre)
       ;; XXX Not exactly lexprs, because we really go into the state AFTER the hd
       (append* (parse-lexprs (cons ': pre)))]
      [(check1 #\|)
       (define col (loc-col (current-loc ip)))
       (define prep (cons col pre))
       (cons (cons '#%bar
                   (cons (parse-lexpr (cons 'first-bar prep) #t)
                         (parse-lexprs prep)))
             (or (parse-lexpr pre #t)
                 '()))
       ]
      ;; XXX more
      [else
       (parse-error 'ltail pre)]))
  (define ltail-start-cs
    (string->char-set "\n:&\\|@"))

  (define (parse-lexpr pre ok-to-fail?)
    (cond
      [(not (parse-prefix pre))
       (if ok-to-fail?
         #f
         (parse-error 'lexpr 'pre pre))]
      [else
       (define hd (parse-iexpr ltail-start-cs))
       (eprintf "> hd = ~v\n" hd)
       (define md (parse-q*expr ltail-start-cs))
       (eprintf "> md = ~v\n" md)
       (define tl (parse-ltail pre))
       (eprintf "> tl = ~v\n" tl)
       (cons hd
             (if (null? md) tl
                 (cons md tl)))]))

  ;; XXX Change parser to accrue a stack of "constraints" that are
  ;; imposed whenever a #\newline is seen. Individual expressions are
  ;; yielded so they can be accrued by a structure higher up.
  ;;  \n --- impose the constraints
  ;;   \ --- add the constraint to have an extra indent, the yield function goes to main
  ;;   | --- add the constraint to line up with |, the yield function goes to body
  ;;   : --- add the constraint to have an extra indent, the yield function goes to body
  ;;  @{ --- add an indent, content parsed as text, yield continues
  ;;   @ --- add an indent, content parsed as text
  ;;   & --- (jay) no new constraints, the yield goes to main
  ;;   & --- (mflatt) yield to previous line's
  ;; When a line violates a constraint, we pop the constraint stack and yield the accrued structure


  #|

   parse : mode x constraints x success-cont x fail-cont

   mode = Expression or Text (for '@' mode)

   constraints = a stack of constraints that must be satisfied before
   it is possible to parse an individual expression

   success-cont = Called if the constraints are met AND an iexpr is
   parsed, called with the iexpr

   fail-cont = Called if the constraints are NOT met; no characters
   are read

  |#
  
  (define (parse-lexprs pre)
    (define a (parse-lexpr pre #t))
    (if a
      (cons a (parse-lexprs pre))
      '()))

  (parse-lexpr '() #f))

(module+ test
  (require racket/list
           racket/file
           racket/string
           racket/port
           racket/system
           racket/pretty
           racket/runtime-path)
  (define (value->file f v)
    (with-output-to-file f (λ () (pretty-write v)) #:exists 'replace))

  (define-runtime-path actual.rktd "actual.rktd")
  (define-runtime-path expected.rktd "expected.rktd")
  (define (parse-test label in expected)
    (define actual (call-with-input-string in parse))
    (unless (equal? actual expected)
      (eprintf "Test failed: ~a\n" label)
      (displayln in)
      (value->file actual.rktd actual)
      (value->file expected.rktd expected)
      (system* (find-executable-path "diff") "-u"
               actual.rktd expected.rktd)
      (newline)))

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
      (parse-test (format "extracted L~a" le-line) lexpr (with-input-from-string sexpr read))
      (extract-tests after-line after)))

  (define-runtime-path md "../0004-lexpr.md")
  (extract-tests 0 (file->lines md)))

#|

   Goal: Syntax with only one way to write things an AST.

   Problem: Lining up definitions or other things, like:
      let foo = 1
            a = 2
   Solution 1: Allow multiple |s?
      let | foo | 1
          |   a | 2
   Problem 1.1: Formatting is annoying to maintain and must be fixed
   up.
   Modification 1.2: Treat = (and =>?) as another kind of balancer,
   like | because it is common.

   Problem: Comments
   Solution 1: Don't have comments and insist on literal programming
   for prose, plus logging, tests ("Show, don't tell"), good names,
   specification, etc, because the compiler doesn't execute or analyze
   comments.
   Problem 1.a: What about commenting out code?
   Solution 1.a.1: (when false ...) is easy, but doesn't work for
   macros.
   Solution 1.a.2: `git diff` and the kill ring has the knowledge you
   want


|#
