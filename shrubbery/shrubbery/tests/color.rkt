#lang racket/base
(require racket/class
         racket/string
         syntax-color/racket-lexer
         shrubbery/lex
         shrubbery/lex-comment
         shrubbery/lex-open-close
         "like-text.rkt"
         "input.rkt")

(define (lex-all-input in fail
                       #:keep-type? [keep-type? #t]
                       #:error-ok? [error-ok? #f])
  (let loop ([status #f])
    (define-values (start-line start-column start-offset) (port-next-location in))
    (define-values (r type paren start-pos end-pos backup new-status)
      (lex/open-close/status in 0 status racket-lexer*/status))
    (define-values (end-line end-column end-offset) (port-next-location in))
    (unless error-ok?
      (when (eq? type 'error)
        (error "color lexer reported error" (list start-line start-column))))
    (cond
      [(eq? type 'eof)
       null]
      [else
       (define tok (if (token? r)
                       r
                       (syntax->token 'other
                                      (datum->syntax #f
                                                     r
                                                     (srcloc #f
                                                             start-line
                                                             start-column
                                                             start-offset
                                                             (- end-offset start-offset)))
                                      (list start-line
                                            start-column
                                            start-offset
                                            (- end-offset start-offset)))))
       (cons (if keep-type?
                 (vector tok type paren)
                 tok)
             (loop new-status))])))

(define (lex-all-input/error-ok in fail #:keep-type? [keep-type? #t])
  (lex-all-input in fail
                 #:keep-type? keep-type?
                 #:error-ok? #t))

;; Check that the color lexer doesn't crash, even if the input is ill-formed
(define (color-test which str)
  (printf "coloring ~s\n" which)
  (new like-text%
       [lex-all-input lex-all-input]
       [content str])
  ;; try dropping (random) characters:
  (define try-all? ((string-length str) . < . 2000))
  (for ([i (in-range 0 #;(if try-all? (string-length str) 200))])
    (define pos (if try-all?
                    i
                    (random (string-length str))))
    (with-handlers ([exn:fail? (lambda (exn)
                                 (log-error "fail at ~s" pos)
                                 (raise exn))])
      (new like-text%
           [lex-all-input lex-all-input/error-ok]
           [content (string-append
                     (substring str 0 pos)
                     (substring str (add1 pos)))]))))

(color-test 1 input1)
(color-test "1a" input1a)
(color-test "1b" input1b)
(color-test 2 input2)
(color-test 3 input3)
(color-test 4 input4)
(color-test 5 input5)
(color-test 6 input6)
(color-test 7 input7)
(color-test 8 input8)

(define (add-prefix s prefix)
  (apply string-append (map (lambda (s) (string-append prefix s "\n"))
                            (string-split s "\n"))))

(color-test "1t" (add-prefix input1 "\t"))

;; Check tracking of comment regions.
;; The "^"s here show the range of commenting. Each "^" will be stripped
;; to produce the actual input.
(define example1
#<<INPUT
{
  hello:
    val x:
      #//
      ^g(-1)^
      f(
        #//
        ^0^,
        1,
        2 + 3,
        #//
        ^4 + 5^)
    #//
    ^not included in the code^
    match x
    #//
    ^| 0: no^
    | 1: 'one'
    #//
    ^| 1.5: no^
    | 2: 'two'
    #//
    ^| 3: no^,
  #//
  ^goodbye:
    the enclosing group of the block is commented out^
}

/* this is a normal comment, not dimmed */
// this is also a normal comment, not dimmed

{
  hello:
    val x:
      #// ^g(-1)^
      f(#// ^0^, 1, 2 + 3, #// ^4 + 5^)
    #// ^not included in the code^
    match x #// ^| 0: no^ | 1: 'one' #// ^| 1.5: no^
                | 2: 'two' #// ^| 3: no^,
  #// ^goodbye:
    the enclosing group of the block is commented out^
}

#// ^#{#(use-s-exp
reader
"all sorts of stuff")}^
kept
f(#//^#{#(,unquote)}^, 2)

#{#(1 #;^(1 2)^ 3)}

@hello(
  1
  #//
  ^2^
  3
)

@a{@//^{[apple}^ 2}
@a{@//anything
   more}

@a{@//^{@a[apple]{banana}}^ 2}
@a{@//^{@a[apple]{banana @//{nested}}}^ 2}
@a{@//^{@a[apple]{banana @//{@[#//nested]}}}^ 2}
@a{@a[apple]{banana @[#//^nested^]} 2}
ok
#//
^@a{@//{[apple]} 2}^
ok

;«
#//
^one^
^two^;
three
»

#//
^a^
^| x^
(the end)

#//
^a^
^| x^
// the end
done

#//
^// not the end^
^a^

(1,
 #//
^// not the end^
 ^a^)

(1,
 #//
 ^2^
 // not the end, as it turns out
   ^+ a^)

a
| x
#//
^| y^
| z
(the end)

a
| x
| y
#//
^| z^
(the end)

'«a
  #//
  ^b^
  c»'
(the end)

;«
   a
   #//
   ^| x^
   ^b^
   ^c^
 »
(the end)

;«
   '«a
     #//
     ^| x^
     ^b^
     ^c^»'
 »
(the end)

INPUT
)
;; ^#// #{#(1 @;(1 2) 3)}^

(define clean-example1 (regexp-replace* #rx"\\^" example1 ""))

(define t (new like-text% [content clean-example1]
               [lex-all-input lex-all-input]))

(printf "comments\n")
(let loop ([i 0] [j 0] [comment? #f])
  (cond
    [(= i (string-length example1))
     (void)]
    [(eqv? #\^ (string-ref example1 i))
     (loop (add1 i) j (not comment?))]
    [else
     (define c (send t classify-position* j))
     (define-values (s e) (send t get-token-range j))
     (unless (eq? (if (hash? c) (hash-ref c 'type #f) c) 'white-space)
       (unless (eq? comment? (if (hash? c)
                                 (hash-ref c 'comment? #f)
                                 #f))
         (error 'color-test "wrong ~s at offset ~s" (not comment?) i)))
     (loop (add1 i) (add1 j) comment?)]))

;; Check invisible opens and closes; `<` is open, `>` is close,
;; and `<`, `>`, and `_` are all stripped before lexing
(define example2
#<<INPUT
_<(<1>, <2>, <3>)>

_<[<1>,
__ <2>,
__ <3>]>

_<'<<1>; ;
__ _<2>
__ _; <3>>'>

_<f(<1>, <2>, <3>)>

_<1 + 2
__   + 3>

_<1 + 2
__   + 3
__   + 4>

_<'<<«<<1 2>
__ __ _<3>>»>>'>

_<a:
__  <<1 2>_
__  _<3 4>>>

_<b
__| <<y>>>

_<b
__| <<x>>
__| <<y>>>

_<b
__| <<x1>
__  _<x2>
__  _<x3>>
__| <<y>
__  _<y2>>>

_<c
__| <<d1:
__  __  <<x1>
__  __  _<x2>
__  __  _<x3>>>>>

_<c
__| <<d1:
__  __  <<x1>
__  __  _<x2>
__  __  _<x3>>>>
__| <<y>
__  _<y2>>>

_<x | <<1>> | <<2 3>>>

_<x | <<1>> | <<2 3>>
__  | <<4>>>

_<x |
__    <<1>>
__  |
__    <<2 3>>>

_<(<| <<2>>>)>

_<(<|>)>

_<(<| <<3>>>,
__ <6>)>

_<m:
__  <<n>>
__| <<o>
__  _<p>>>

_<m:
__| <<o>
__  _<p>>>

_<m:
__  <<n>
__// comment
__// comment
__  _<p>>>

_<a:
__  <<1> ; <x:
__  __ _   _  <<y>>>
__  _<3>>>

_<(<1>,
__ <2>, <3>,
__ <4>)>

_<(<1>
__,
__ <2>  ,  <3>
__ _ _     _ _   ,
__ <4>)>

_<x:<<«<<1 2
__  __ __3>>»>>>

_<x:<<«<<1 2
__  3>>»>>>

_<x:  <<«<<1 2
__    __ __3>>
__»>>>

_<x
__|<<«<<1 2
__  __ _ 3>>»>>>

_<1 + 2
__   + 3:
__  <<4>>>

_<@{}>

_<@{ hello
__   there }>

_<@{ hello <@(<1 + 2>)>
__   there }>

_<block:
__  <<1+2>
__  _<3+4>>>

_<class Vec3:
__  <<method: <<1>; <2>>>;
__  _<private:
__  __  <<error>>>>>

_<done>_
INPUT
  )

(define clean-example2 (regexp-replace* #rx"[<>_]" example2 ""))

(define t/oc (new like-text% [content clean-example2]
                  [lex-all-input lex-all-input]))

(printf "invisible opens and closes\n")
(let loop ([i 0] [j 0] [expected-opens 0])
  (cond
    [(= i (string-length example2))
     (void)]
    [(eqv? #\< (string-ref example2 i))
     (loop (add1 i) j (add1 expected-opens))]
    [(eqv? #\_ (string-ref example2 i))
     (loop (add1 i) j expected-opens)]
    [else
     (define c (send t/oc classify-position* j))
     (define-values (s e) (send t/oc get-token-range j))
     #;(log-error "<i, j> = <~a, ~a> ~s" i j (substring example2 i (+ i (- e s))))
     (define opens (if (hash? c) (hash-ref c 'invisible-open-count 0) 0))
     (define closes (if (hash? c) (hash-ref c 'invisible-close-count 0) 0))
     (unless (eqv? opens expected-opens)
       (error 'color-test "wrong opens ~s != ~s at offset ~s; ~s" opens expected-opens i c))
     (define delta (- e s))
     (let ([j (+ j delta)]
           [i (let loop ([i i] [delta delta]) ; advance by `delta` not counting `_`s
                (cond
                  [(= delta 0) i]
                  [(eqv? #\_ (string-ref example2 i)) (loop (add1 i) delta)]
                  [else (loop (add1 i) (sub1 delta))]))])
       (let c-loop ([i i] [expected-closes 0])
         (cond
           [(and (< i (string-length example2))
                 (eqv? #\> (string-ref example2 i)))
            (c-loop (add1 i) (add1 expected-closes))]
           [else
            (unless (eqv? closes expected-closes)
              (error 'color-test "wrong closes ~s != ~s at offset ~s" closes expected-closes i))
            (loop i j 0)])))]))

"passed"
