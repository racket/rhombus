#lang racket/base
(require racket/pretty
         "../lex.rkt"
         "../parse.rkt"
         "../print.rkt"
         "../write.rkt"
         "input.rkt")

(define (check which input expected)
  (printf "checking ~s\n" which)
  (let ([in (open-input-string input)])
    (define (out name parsed write)
      (define path (build-path (find-system-path 'temp-dir) name))
      (printf "~a\n" path)
      (call-with-output-file*
       path
       #:exists 'truncate
       (lambda (o) (write parsed o))))
    (port-count-lines! in)
    (define parsed-stx (parse-all in))
    (define parsed (syntax->datum parsed-stx))
    (unless (equal? expected parsed)
      (out "expected" expected pretty-write)
      (out "parsed" parsed pretty-write)
      (error "parse failed"))
    (define printed (shrubbery-syntax->string parsed-stx))
    (unless (equal? input printed)
      (out "expected" input display)
      (out "printed" printed display)
      (error "print failed"))
    (define (check-reparse mode)
      (define (add-newlines bstr)
        (define in (open-input-bytes bstr))
        (define out (open-output-bytes))
        (display ";«" out)
        (for ([tok (in-list (lex-all in error))])
          (define loc (token-srcloc tok))
          (define pos (sub1 (srcloc-position loc)))
          (display (subbytes bstr pos (+ pos (srcloc-span loc))) out)
          (newline out))
        (display "»" out)
        (get-output-bytes out))
      (define reparsed (let ([o (open-output-bytes)])
                         (write-shrubbery parsed o)
                         (define new-in (let ([bstr (get-output-bytes o)])
                                          (cond
                                            [(eq? mode 'add-newlines) (add-newlines bstr)]
                                            [else bstr])))
                         (or (with-handlers ([exn:fail? (lambda (exn) (eprintf "~a\n" (exn-message exn)) #f)])
                               (define in (open-input-bytes new-in))
                               (when (eq? mode 'count)
                                 (port-count-lines! in))
                               (syntax->datum (parse-all in)))
                             (begin
                               (out "wrote" new-in displayln)
                               (error "parse of wrote failed")))))
      (unless (equal? parsed reparsed)
        (out "expected" parsed pretty-print)
        (out "reparsed" reparsed pretty-print)
        (error "print failed")))
    (check-reparse 'count)
    (check-reparse 'no-count)
    (check-reparse 'add-newlines)))

(define (check-fail input rx)
  (let ([in (open-input-string input)])
    (port-count-lines! in)
    (unless (with-handlers ([exn:fail? (lambda (exn) (regexp-match? rx (exn-message exn)))])
              (parse-all in)
              #f)
      (error "failed to fail: ~s" input))))

(define (lines s . ss)
  (apply string-append s (for/list ([s (in-list ss)]) (string-append "\n" s))))

(check 1 input1 expected1)
(check '1a input1a expected1a)
(check 2 input2 expected2)
(check 3 input3 expected3)
(check 4 input4 expected4)

(check-fail "x:" #rx"empty block")
(check-fail "x:\ny" #rx"empty block")
(check-fail "x |" #rx"empty block")
(check-fail "x |\ny" #rx"empty block")
(check-fail "(x:)" #rx"empty block")
(check-fail "(1, x:)" #rx"empty block")

(check-fail "if t | «tag: if f | a | b» more | y" #rx"no terms allowed after `»`")
(check-fail "x: y:« a; b » more; c" #rx"no terms allowed after `»`")

(check-fail (lines "x"
                   " y")
            #rx"wrong indentation")
(check-fail (lines "1"
                   " + 2"
                   "   + 3")
            #rx"wrong indentation")
(check-fail (lines "1: 2"
                   " + 3")
            #rx"wrong indentation")
(check-fail (lines "x | y | c"
                   "  + 3")
            #rx"wrong indentation")

(check-fail "a:\n«c»" #rx"not on the same line")
(check-fail "a |\n«c»" #rx"not on the same line")
(check-fail ";\n«c»" #rx"not on the same line")

(check-fail "(«| a | c»)" #rx"misplaced `«`")
(check-fail "z «|« « | y » » »" #rx"misplaced `«`")
(check-fail "«|« w « | y » » »" #rx"misplaced `«`")

(check-fail (lines "(#// x,"
                   "     y)") #rx"wrong indentation")
(check-fail (lines "(#//"
                   "   x,"
                   "   y)") #rx"wrong indentation")
(check-fail (lines "cond"
                   "  #// | x") #rx"wrong indentation")
(check-fail (lines "cond"
                   "#// | x") #rx"misplaced `[|]`")
(check-fail "x #// y" #rx"misplaced group comment")

(check-fail "a(;«1» 2)" #rx"comma")
(check-fail "a(;«1; 2», 2)" #rx"multi-group splice")
