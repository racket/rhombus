#lang racket/base
(require racket/match
         srfi/14)

(struct loc (line col pos) #:transparent)
(define (current-loc ip)
  (call-with-values (λ () (port-next-location ip)) loc))

(define (parse ip)
  (port-count-lines! ip)

  (define (parse-error state . params)
    (error 'parse-error "~a: ~v => unexpected ~v: ~e" state params (peek-char ip) (read-bytes 128 ip)))

  (define (parse-prefix pre)
    (match pre
      ['() (void)]))

  (define (parse-whitespace)
    (cond
      [(regexp-try-match #rx"^ +" ip)
       #t]
      ;; XXX comments
      [else
       #f]))

  (define (parse-iexpr stop-re)
    (cond
      [(regexp-try-match #rx"^[^ ]*" ip)
       => (λ (m)
            (string->symbol (bytes->string/utf-8 (car m))))]
      [else
       (parse-error 'iexpr)]))
  
  (define (parse-qexpr stop-re)
    (cond
      [(regexp-try-match stop-re ip)
       '()]
      [else
       (cons (parse-iexpr stop-re) (parse-q*expr stop-re))]))

  (define (parse-q*expr stop-re)
    (if (parse-whitespace)
      (parse-qexpr stop-re)
      '()))

  (define (parse-ltail pre)
    (cond
      [else
       (parse-error 'ltail pre)]))
  (define ltail-start-re
    #rx"^(\n|:|&|\\\\|\\||@)")

  (define (parse-lexpr pre)
    (parse-prefix pre)
    (define hd (parse-iexpr))
    (define md (parse-q*expr ltail-start-re))
    (define tl (parse-ltail pre))
    (cons hd (append md tl)))

  (define (parse-lexprs pre)
    (define a (parse-lexpr pre))
    (if a
      (cons a (parse-lexprs pre))
      '()))

  (parse-lexpr '()))

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
