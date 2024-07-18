#lang at-exp racket/base
(require racket/class
         shrubbery/lex
         shrubbery/indentation
         "like-text.rkt"
         (for-syntax racket/base))

(define failed? #f)

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
      ^x}
 @e{a:
      y
    ^ ^x}
 @e{a: y
    ^  ^x}

 @e{:
    ^ ^x}

 @e{apple:
      ^x}

 @e{apple: banana:
             ^x}
 @e{apple: banana:
             y
    ^      ^ ^x}

 @e{apple: :
             ^x}

 @e{(a:
       ^x}

 @e{(:
     ^ ^x}

 @e{[a:
       ^x}

 @e{[:
     ^ ^x}

 @e|{{a:
        ^x}|

 @e|{{:
      ^ ^x}|

 @e{define pi:
      ^x}

 @e{define pi: 3.14
    ^          ^x}

 @e{define fib(n):
     cond | n == 0: 0
    ^     ^ ^       ^|}

 @e{define fib(n):
     cond | n == 0:
              0
    ^     ^ ^ ^|}

 @e{define fib(n):
     log_error("fib called")
     cond | n == 0: 0
    ^     ^ ^       ^|}
 
 @e{define
     | fib(0): 0
     | fib(1): 1
     | fib(n): fib(n-1) // in parens => `+` continues this line
    ^  ^       ^ ^+ fib(n-2))}

 @e{define
     | fib(0): 0
     | fib(1): 1
     | fib(n): (fib(n-1) // in parens => `+` continues this line
                ^ ^+ fib(n-2))}

 @e{define fib:
     lambda (n):
       cond
       | n == 0: 0
    ^^ ^ ^       ^| n == 1: 1}

 @e|{define fib(n):
      match n { | 0 { 0 }
                ^| 1 { 1 }}|

 @e{1 + x: « 3
             ^4 »}

 @e{1 + x: « 3
             ^ ^+ 4 »}
                
 @e{1 + x: 3
    ^      ^ ^+ 4}

 @e{x: 1 + x: 3
    ^  ^      ^ ^+ 4}
                
 @e{x: 1 + x | 3
    ^  ^       ^ ^+ 4}
                
 @e{define analyze(n):
      if n == 0
      | printf("zero\n")
    ^ ^ ^printf}

 @e{define go():
      hello
      | world
    ^ ^ ^|}

 @e{define go():
      define more(m):
        if m == 0 | "done"
    ^ ^           ^ ^|}

 @e{define go():
      define more(m):
        if m == 0 | "done"
    ^ ^ ^           ^x}


 @e{define approx_thunk(x):
      match x
      | something(v): lambda
                      | (): v
    ^ ^ ^             ^ ^   ^| (n): v+n}

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
            ^ ^    ^| two}

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
                     ^d}

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

 @e{x + w | z : : x
    ^       ^   ^ ^y}
 @e{x + w | z : :
                  ^y}
 @e{x + w | z : q:
                  ^y}

 @e{x something | a
                  y:
                    w:
                      ^q}
 @e{x something | a
                  y:
                    w:
                      z
    ^             ^ ^ ^q}

 @e{z: something | x
    ^  ^           ^y}
 @e{z: q | x
    ^  ^   ^y}

 @e{z:
    | x
    ^ ^y}
 @e{z: r
       | x
    ^  ^ ^y}

 @e|{|z {
        | x
          ^y|}|

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

 @e{(match v
     | cons(bv, av):
         define values(is_a_match, ar):
           ¿a_pred(av)
         if is_a_match
     ^ ^ ^| define}

 @e{a  |« b»
    ^ c}

 @e{a: x
    ^  ^#//}
 @e{a: #// b
    ^  ^x}
 @e{a: (#// b,
        ^x}
 @e{a: (#//
        ^x}
 @e{a: #// x: q
    ^  ^      ^x} ; would be nice to not include that first stop
 @e{a: #// x: q
       w
    ^  ^x}
 @e{a: #// x: y
    ^  ^      ^x}
 @e{a: #// | x
           ^ ^|}
 @e{a: x | x #// | y
         ^|}
 @e{a: x | x
    ^  ^   ^#//}
 @e{a: | x
    #//
       ^ ^|}
 @e{b | x (x | y)
      ^ ^| me}
 @e{b | (x | y)
      ^ ^| me}
 @e{b | x | y
      ^| me}

 @e{b | 
        ^me}
 @e{b | x
    ^   ^me}

 @e{a(x,
      ^+1}
 @e{a: x;
    ^  ^+1}
 @e{a:«x»
    ^x}
 @e{z:
      a:«x»
      b
    ^ ^|x}

 @e{(1,
     a:
       ^x}

  @e{apple(
     ^)}
  @e{apple(1,
           ^)}
  @e{apple(
       1,
     ^)}
  
  @e{(apple:
        nested
      ^)}
  @e{(more (apple:
              nested
            ^)}

  @e{(1,
      apple
      ^|}
  @e{(1,
      apple
      ^ ^+}
  @e{[1,
      ^apple}

  @e{match 1:
     | x:
         x
     ^ ^ ^x}

  @e{1
     ^//
     2}
  @e{1
     ^ ^//
       + 2}
  @e{(1
      ^ ^//
        + 2)}

  @e{block:
       1
     ^ ^//
       2}
  @e{block:
       1
     ^ ^ ^//
         + 2}

  @e{match 1
     | x: x
     ^ ^  ^//
          2}
  @e{match 1
     | x: x
     ^ ^  ^ ^//
            + 2}
 )

(when failed?
  (error "failed"))
