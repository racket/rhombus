#lang racket/base

(define (parse ip)
  'XXX)

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
      (value->file actual.rktd actual)
      (value->file expected.rktd expected)
      (system* (find-executable-path "diff") "-u"
               actual.rktd expected.rktd)
      (newline)))

  (define-runtime-path x-in "x.txt")
  (define-runtime-path x-out "x.rktd")
  (parse-test "x" (file->string x-in) (file->value x-out))

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
