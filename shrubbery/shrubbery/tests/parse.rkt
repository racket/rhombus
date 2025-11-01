#lang racket/base
(require racket/pretty
         racket/string
         shrubbery/lex
         shrubbery/parse
         shrubbery/print
         shrubbery/write
         shrubbery/private/simple-pretty
         "input.rkt")

(define (check which input expected #:check-print? [check-print? #t])
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
    (check-structure parsed-stx)
    (define parsed (syntax->datum parsed-stx))
    (unless (equal? expected parsed)
      (out "expected" expected pretty-write)
      (out "parsed" parsed pretty-write)
      (error "parse failed"))
    (define printed (shrubbery-syntax->string parsed-stx #:keep-prefix? #t #:keep-suffix? #t))
    (when check-print?
      (unless (equal? input printed)
        (out "expected" input display)
        (out "printed" printed display)
        (error "print failed")))
    (define (check-reparse mode #:add-newlines? [add-newlines? #f])
      (define (add-newlines bstr)
        (define in (open-input-bytes bstr))
        (define out (open-output-bytes))
        (unless (memq mode '(pretty-multi pretty-prefer-multi pretty-prefer-multi2))
          (display ";«" out))
        (for ([tok (in-list (lex-all in error))])
          (define loc (token-srcloc tok))
          (define pos (sub1 (srcloc-position loc)))
          (define raw (subbytes bstr pos (+ pos (srcloc-span loc))))
          (display raw out)
          (newline out))
        (unless (memq mode '(pretty-multi pretty-prefer-multi pretty-prefer-multi2))
          (display "»" out))
        (get-output-bytes out))
      (define new-in
        (let ([o (open-output-bytes)])
          (cond
            [(eq? mode 'pretty)
             (define doc `(seq ,@(for/list ([g (in-list (cdr parsed))])
                                   `(seq ,(pretty-shrubbery g) nl))))
             (render-pretty doc o #:width (if (or (eq? mode 'pretty-multi)
                                                  (eq? mode 'pretty-multi-armored))
                                              0
                                              #f))]
            [(eq? mode 'pretty-one)
             (write-shrubbery parsed o #:pretty? #t)]
            [(eq? mode 'pretty-multi)
             (write-shrubbery parsed o #:pretty? #t #:width 0)]
            [(eq? mode 'pretty-multi-armored)
             (write-shrubbery parsed o #:pretty? #t #:width 0 #:armor? #t)]
            [(eq? mode 'pretty-prefer-multi)
             (write-shrubbery parsed o #:pretty? #t #:prefer-multiline? #t)]
            [(eq? mode 'pretty-prefer-multi2)
             (write-shrubbery parsed o #:pretty? #t #:width 0 #:prefer-multiline? #t)]
            [else
             (write-shrubbery parsed o)])
          (let ([bstr (get-output-bytes o)])
            (cond
              [add-newlines?
               (add-newlines bstr)]
              [else bstr]))))
      (define reparsed
        (or (with-handlers ([exn:fail? (lambda (exn) (eprintf "~a\n" (exn-message exn)) #f)])
              (define in (open-input-bytes new-in))
              (when (or (eq? mode 'count)
                        (eq? mode 'pretty)
                        (eq? mode 'pretty-multi))
                (port-count-lines! in))
              (syntax->datum (parse-all in)))
            (begin
              (out "wrote" new-in displayln)
              (error "parse of wrote failed" mode))))
      (unless (equal? parsed reparsed)
        (out "expected" parsed pretty-print)
        (out "reparsed" reparsed pretty-print)
        (out "wrote" new-in display)
        (error "print failed" mode)))
    (check-reparse 'count)
    (check-reparse 'no-count)
    (check-reparse 'count #:add-newlines? #t)
    (check-reparse 'pretty)
    (check-reparse 'pretty-one)
    (check-reparse 'pretty-one #:add-newlines? #t)
    (check-reparse 'pretty-multi)
    (check-reparse 'pretty-multi-armored)
    (check-reparse 'pretty-multi-armored #:add-newlines? #t)
    (check-reparse 'pretty-prefer-multi)
    (check-reparse 'pretty-prefer-multi2)))

(define (check-fail input rx)
  (let ([in (open-input-string input)])
    (port-count-lines! in)
    (unless (with-handlers ([exn:fail? (lambda (exn)
                                         (or (regexp-match? rx (exn-message exn))
                                             (error 'check-fail
                                                    "not expected error: ~s\n  input: ~s"
                                                    (exn-message exn)
                                                    input)))])
              (parse-all in)
              #f)
      (error 'check-fail "failed to fail: ~s" input))))

;; checks for a 'identifier-as-keyword property on every head identifier
(define (check-structure parsed-stx)
  (let loop ([parsed-stx parsed-stx] [top? #t] [head? #f])
    (cond
      [(identifier? parsed-stx)
       (case (and head? (syntax-e parsed-stx))
         [(group block alts quotes parens brackets braces op multi)
          (unless (syntax-property parsed-stx 'identifier-as-keyword)
            (error "not tagged as keyword-like identifier" parsed-stx))]
         [else
          (when head?
            (error "unexpected head term" parsed-stx))
          (when (syntax-property parsed-stx 'identifier-as-keyword)
            (error "tagged as keyword-like identifier" parsed-stx))])]
      [head?
       (error "unexpected head term" parsed-stx)]
      [(syntax? parsed-stx)
       (define e (syntax-e parsed-stx))
       (cond
         [(and top? (pair? e))
          (loop (car e) #t #t)
          (loop (cdr e) #f #f)]
         [else (loop e #f #f)])]
      [(pair? parsed-stx)
       (loop (car parsed-stx) #t #f)
       (loop (cdr parsed-stx) #f #f)]
      [else (void)])))

(define (lines s . ss)
  (apply string-append s (for/list ([s (in-list ss)]) (string-append "\n" s))))

(define (add-prefix s prefix)
  (apply string-append (map (lambda (s) (string-append prefix s "\n"))
                            (string-split s "\n"))))

(check 1 input1 expected1)
(check '1a input1a expected1a)
(check '1b input1b expected1b)
(check '1t (add-prefix input1 "\t") expected1 #:check-print? #f)
(check '1tsts (add-prefix input1 "\t  \t ") expected1 #:check-print? #f)
(check '1at (add-prefix input1a "\t") expected1a #:check-print? #f)
(check '1bt (add-prefix input1b "\t") expected1b #:check-print? #f)
(check 2 input2 expected2)
(check 3 input3 expected3)
(check 3 input3b expected3b #:check-print? #f)
(check 4 input4 expected4)
(check 5 input5 expected5)
(check 6 input6 expected6)
(check 7 input7 expected7)
(check 8 input8 expected8)
(check 9 input9 expected9)

(check 'mix (string-append "x:\n"
                           "  apple\tbanana\n"
                           "  coconut\n"
                           "y:\n"
                           "  1\t(2,\n"
                           "   \t 3)\n"
                           "z:\n"
                           "\t  \t A\n"
                           "\t  \t B(C,\n"
                           "\t  \t   D:\t\t E\n"
                           "\t  \t     \t\t F)\n"
                           "w:\n"
                           "\t1\n"
                           "| 2\n"
                           "q\n"
                           "| \t1\n"
                           "  \t| 1.1\n"
                           "  \t| 1.2\n"
                           "| 2")
       '(multi
         (group x (block (group apple banana) (group coconut)))
         (group y (block (group 1 (parens (group 2) (group 3)))))
         (group z (block (group A)
                         (group B (parens (group C)
                                          (group D (block (group E)
                                                          (group F)))))))
         (group w (block (group 1)) (alts (block (group 2))))
         (group q (alts (block (group 1
                                      (alts (block (group 1.1)) (block (group 1.2)))))
                        (block (group 2))))))

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
(check-fail (lines "(x, y,"
                   "    z)")
            #rx"wrong indentation")
(check-fail (lines "(x, y,"
                   " | z)")
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

(check-fail "@«1, 2»" #rx"second group not allowed")
(check-fail "@(«1, 2»)" #rx"second group not allowed")
(check-fail "@(«1 2» )" #rx"expected a closing `[)]` immediately after closing `»`")
(check-fail "@x[1]{2}" #rx"cannot start `[[]`")
(check-fail "@«1: 2»()" #rx"block not allowed")
(check-fail "@«1: 2» x" #rx"block not allowed")
(check-fail "@(«1: 2»)x" #rx"block not allowed")
(check-fail "@«| 1» more" #rx"block not allowed")

(check-fail "1x" #rx"read error")
(check-fail "1x_y" #rx"read error")
(check-fail "1.x" #rx"read error")
(check-fail "1.2x" #rx"read error")
(check-fail "1.2ex" #rx"read error")
(check-fail "1.2e5x" #rx"read error")
(check-fail "1.2e5.x" #rx"read error")
(check-fail "1.2e5.0x" #rx"read error")
(check-fail "1.2.3" #rx"read error")
(check-fail "1.2.3." #rx"read error")

(check-fail "~#%call" #rx"read error")

(check-fail "\t1\n 2" #rx"mixed tabs")
(check-fail "\t1\n\t 2" #rx"wrong indentation")
(check-fail " 1\n \t2" #rx"wrong indentation")
(check-fail "\ta\n\t| 1\n | 2" #rx"mixed tabs")
(check-fail "\"x\ty\": 1\n       2" #rx"mixed tabs")

(check-fail "1 \\ 2" #rx"line-continuing '\\\\' is followed by a another token")
(check-fail "1 \\ /* ok */ 2" #rx"line-continuing '\\\\' is followed by a another token")
(check-fail "1 \\ \n 2 \\ 3" #rx"line-continuing '\\\\' is followed by a another token")

(check-fail "a(1,\n   " #rx"did not find matching .[)].")

(check-fail "@@{}" #rx"invalid after `@`")
(check-fail "@@//{}" #rx"invalid after `@`")
