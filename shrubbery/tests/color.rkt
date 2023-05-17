#lang racket/base
(require racket/class
         syntax-color/racket-lexer
         "../lex.rkt"
         "../lex-comment.rkt"
         "like-text.rkt"
         "input.rkt")

(define (lex-all-input in fail
                       #:keep-type? [keep-type? #t]
                       #:error-ok? [error-ok? #f])
  (let loop ([status #f])
    (define-values (start-line start-column start-offset) (port-next-location in))
    (define-values (r type paren start-pos end-pos backup new-status)
      (lex/comment/status in 0 status racket-lexer*/status))
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
                                      r
                                      (srcloc (object-name in)
                                              start-line
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

"passed"
