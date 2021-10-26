#lang racket/base
(require racket/class
         syntax-color/racket-lexer
         "../lex.rkt"
         "../lex-comment.rkt"
         "like-text.rkt")


;; The "^"s here show the range of commenting. Each "^" will be stripped
;; to produce the actual input.
(define example1
#<<INPUT
{
  hello:
    val x:
      ^#//
      g(-1)^
      f(
        ^#//
        0^,
        1,
        2 + 3,
        ^#//
        4 + 5^)
    ^#//
    not included in the code^
    match x
    ^#//
    | 0: no^
    | 1: 'one'
    ^#//
    | 1.5: no^
    | 2: 'two'
    ^#//
    | 3: no^,
  ^#//
  goodbye:
    the enclosing group of the block is commented out^
}

/* this is a normal comment, not dimmed */
// this is also a normal comment, not dimmed

{
  hello:
    val x:
      ^#// g(-1)^
      f(^#// 0^, 1, 2 + 3, ^#// 4 + 5^)
    ^#// not included in the code^
    match x ^#// | 0: no^ | 1: 'one' ^#// | 1.5: no^
                | 2: 'two' ^#// | 3: no^,
  ^#// goodbye:
    the enclosing group of the block is commented out^
}

^#// #{#(use-s-exp
reader
"all sorts of stuff")}^
kept
f(^#//#{#(,unquote)}^, 2)
INPUT
)

(define (lex-all-input in fail #:keep-type? [keep-type? #t])
  (let loop ([status 'initial])
    (define-values (start-line start-column start-offset) (port-next-location in))
    (define-values (r type paren start-pos end-pos backup new-status)
      (lex/comment/status in (file-position in) status racket-lexer/status))
    (define-values (end-line end-column end-offset) (port-next-location in))
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
     (unless (eq? (if (hash? c) (hash-ref c 'type #f) 'white-space) 'white-space)
       (unless (eq? comment? (if (hash? c)
                                 (hash-ref c 'comment? #f)
                                 #f))
         (error 'color-test "wrong ~s at offset ~s" (not comment?) i)))
     (loop (add1 i) (add1 j) comment?)]))
