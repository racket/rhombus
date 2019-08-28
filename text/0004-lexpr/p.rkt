#lang racket/base
(require racket/match
         racket/list
         racket/set
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax)
         racket/generic)

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


(struct loc (line col pos) #:transparent)
(define (current-loc ip)
  (call-with-values (λ () (port-next-location ip)) loc))

;; XXX Change parser to accrue a stack of "constraints" that are
;; imposed whenever a #\n is seen. Individual expressions are
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

(struct set-complement (s)
  #:methods gen:set
  [(define/generic super-set-member? set-member?)
   (define (set-member? st i)
     (not (super-set-member? (set-complement-s st) i)))])
(define (string->set s) (list->set (string->list s)))

(define ((set-mem s) v) (set-member? s v))

(define indent-amount 2)
(define line-follower (set-union (string->set "]\n|:@&\\") (set eof)))
(define follower (set-union line-follower (string->set " .,'()[]<>{}") (set eof)))
(define number-follower (set-remove follower #\.))
(define number-leader (string->set "-0123456789"))
(define text-follower (set-union (string->set "@{}\n") (set eof)))

(define (#%dot-list x y)
  (list* '#%dot x
         (match y
           [(cons '#%dot y) y]
           [_ (list y)])))

(define (read-lexpr ip)
  (port-count-lines! ip)

  (define (parse-error . state)
    (error 'parse-error "~v => unexpected ~v: ~e" state (peek-char ip) (read-bytes 128 ip)))
  (define (spy . state)
    #;(eprintf "spy: ~v: ~e\n"
             state
             (peek-bytes 128 0 ip))
    (void))

  (define (readc-while s)
    (match (peek-char ip)
      [(? (set-mem s)) (cons (read-char ip) (readc-while s))]
      [_ '()]))
  (define (readc-until s)
    ;; XXX Make this more efficient by computing position and doing read-chars
    (readc-while (set-complement s)))
  (define (reads-until pred)
    (list->string (readc-until pred)))
  (define (expectc x)
    (define y (peek-char ip))
    (if (equal? x y)
      (read-char ip)
      (parse-error 'expectc x)))

  (define (text-mode)
    (list '#%text (text-mode* 0)))
  (define (text-mode* braces)
    (define s (reads-until text-follower))
    (match (peek-char ip)
      [(? eof-object?) (parse-error 'text-mode braces)]
      [#\newline (read-char ip)
       (cons (text-single s)
             (text-mode* braces))]
      [#\{ (read-char ip)
       (text-cons (text-cons1 s "{")
                  (text-mode* (add1 braces)))]
      [#\} (read-char ip)
       (if (zero? braces)
         (text-cons (text-single s)
                    '())
         (text-cons (text-cons1 s "}")
                    (text-mode* (sub1 braces))))]
      [#\@ (read-char ip)
       (text-cons (text-cons1 s (list '#%text-esc (unit)))
                  (text-mode* braces))]))
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
       (string->number (reads-until number-follower))]
      [#\' (read-char ip)
       (list '#%quote (unit))]
      [#\# (read-char ip)
       (expectc #\\)
       (read-char ip)]
      [#\( (read-char ip)
       (grouped)]
      [#\{ (read-char ip)
       (text-mode)]
      [#\[ (read-char ip)
       (list '#%quote-line (line #:quoted? #t))]
      [(or #\< #\>)
       (string->symbol (string (read-char ip)))]
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
       (after-leader (list '#%fun-app l (seq #\))))]
      [#\[ (read-char ip)
       (after-leader (list '#%member l (seq #\])))]
      [#\< (read-char ip)
       (after-leader (list '#%param l (seq #\>)))]
      [#\{ (read-char ip)
       (after-leader (list '#%text-app l (text-mode)))]
      [_ l]))

  (define (unit)
    (after-leader (leader)))

  ;; XXX Merge seq and grouped?
  (define (seq endc)
    (match (peek-char ip)
      [(== endc) (read-char ip)
                 '()]
      [_
       (cons (unit) (seq-tail endc))]))
  (define (seq-tail endc)
    (match (peek-char ip)
      [(== endc) (read-char ip)
                 '()]
      [#\, (read-char ip)
       (expectc #\space)
       (cons (unit) (seq-tail endc))]))

  (define (grouped)
    (match (peek-char ip)
      [#\) (read-char ip)
       '()]
      [_
       (cons (unit) (grouped-tail))]))
  (define (grouped-tail)
    (match (peek-char ip)
      [#\) (read-char ip)
       '()]
      [#\space (read-char ip)
       (cons (unit) (grouped-tail))]))

  (define (expect-prefix left-col)
    (spy 'expect-prefix  left-col)
    (cond
      [(for/and ([i (in-range left-col)])
         (equal? #\space (peek-char ip i)))
       (read-string left-col ip)
       #t]
      [else
       #f]))

  (define (line #:left-col [*left-col #f] #:quoted? [quoted? #f])
    (spy 'line *left-col quoted?)
    (define left-col (or *left-col (loc-col (current-loc ip))))
    (line-start left-col quoted?))
  (define (line-start left-col quoted?)
    (spy 'line-start left-col quoted?)
    (match (peek-char ip)
      [(? (set-mem line-follower)) (line-tail left-col quoted?)]
      [_ (cons (unit) (line-tail left-col quoted?))]))
  (define (line-tail left-col quoted?)
    (spy 'line-tail left-col quoted?)
    (match (peek-char ip)
      [(? eof-object?) '()]
      [#\newline (read-char ip)
       (if (and (not (zero? left-col))
                (expect-prefix left-col))
         (line-start left-col quoted?)
         '())]
      [#\space (read-char ip)
       (match (peek-char ip)
         [#\\ (read-char ip)
          (expectc #\newline)
          (define new-col (+ indent-amount left-col))
          (if (expect-prefix new-col)
            (line-start new-col quoted?)
            '())]
         [#\: (read-char ip)
          (expectc #\newline)
          (define new-col (+ indent-amount left-col))
          (cons
           (list '#%indent
                 (lines #:left-col new-col #:quoted? quoted?))
           (line-start left-col quoted?))]
         [_
          (cons (unit) (line-tail left-col quoted?))])]
      [#\]
       (cond
         [quoted?
          (read-char ip)
          '()]
         [else
          (parse-error 'line-tail quoted?)])]
      [_
       (parse-error 'line)]))

  (define (lines #:left-col [left-col 0] #:quoted? [quoted? #f])
    (spy 'lines left-col quoted?)
    (cond
      [(expect-prefix left-col)
       (match (peek-char ip)
         [(? eof-object?) '()]
         [#\newline (read-char ip)
          (lines #:left-col left-col #:quoted? quoted?)]
         [_ (cons (line #:left-col left-col #:quoted? quoted?)
                  (lines #:left-col left-col #:quoted? quoted?))])]
      [else
       '()]))

  (lines))

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
