#lang at-exp racket/base
(require racket/class
         racket/pretty
         "../lex.rkt"
         "../parse.rkt"
         "../armor.rkt"
         "like-text.rkt"
         "input.rkt")

(define (out name parsed write)
  (define path (build-path (find-system-path 'temp-dir) name))
  (printf "~a\n" path)
  (call-with-output-file*
   path
   #:exists 'truncate
   (lambda (o) (write parsed o))))

(define (check what input expected #:unarmor? [unarmor? #t])
  (printf "checking ~s\n" what)
  (define t (new like-text% [content input]))
  (define length (string-length input))

  (armor-region t 0 length)
  (define in (open-input-string input))
  (port-count-lines! in)
  (define orig-parsed (syntax->datum (parse-all in)))
  (define armored (send t get-text 0 (send t last-position)))
  (define armor-parsed
    (with-handlers ([exn:fail? (lambda (exn)
                                 (printf "~a\n" (exn-message exn))
                                 (out "armored" armored write-string)
                                 (error "parse of armored failed"))])
      (define in (open-input-string armored))
      (port-count-lines! in) ; helps with source locations on error
      (syntax->datum (parse-all in))))
  (unless (equal? orig-parsed armor-parsed)
    (out "original" orig-parsed pretty-write)
    (out "armored" armor-parsed pretty-write)
    (error "parse of armored is different"))

  (when unarmor?
    (define t2 (new like-text% [content armored]))
    (unarmor-region t2 0 (send t2 last-position))
    (define unarmored (send t2 get-text 0 (send t2 last-position)))
    (unless (equal? input unarmored)
      (out "input" input display)
      (out "unarmored" unarmored display)
      (out "armored" armored display)
      (error "unarmored is different from input"))))

(check "simple" "a: 1 2" '(top (group a (block (group 1) (group 2)))))
(check 1 input1 expected1)
(check '1a input1a expected1a #:unarmor? #f)
(check 2 input2 expected2 #:unarmor? #f) ; unarmor not right yet for continuing
(check 3 input3 expected3)
(check 4 input4 expected4)
