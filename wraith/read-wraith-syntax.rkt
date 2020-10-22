#lang racket/base

(provide read-wraith-list
         read-wraith-syntax-list
         wraith-string->sexprs)

(require racket/bool
         racket/list
         racket/match
         syntax/readerr
         syntax/srcloc
         (only-in srfi/1 append-reverse)
         "token.rkt")
(module+ test
  (require rackunit))

;; A Tight is one of:
;;  - Token
;;  - Syntax
;; A Tights is a [Listof Tight]

;; A syntax object that has the "original?" property
;; (borrowed from the scribble reader)
(define orig-stx (read-syntax #f (open-input-string "dummy")))

(define (get-srcloc v)
  (cond [(token? v) (token-srcloc v)]
        [else (build-source-location v)]))
(define (line v) (srcloc-line (get-srcloc v)))
(define (column v) (srcloc-column (get-srcloc v)))
(define ((column>=? c) v) (>= (column v) c))

;; read-error : String Srcloc -> Nothing
(define (read-error str loc #:extra-srclocs [es '()])
  (match-define (srcloc src ln col pos span) loc)
  (raise-read-error str src ln col pos span #:extra-srclocs es))

;; A [Followed X] is a (followed X (U Token Eof))
(struct followed [value after] #:transparent)

;; read-wraith-list : Input-Port -> [Listof S-Expr]
(define (read-wraith-list in)
  (map syntax->datum (read-wraith-syntax-list (object-name in) in)))

;; read-wraith-syntax-list : Any Input-Port -> [Listof Syntax]
(define (read-wraith-syntax-list src in)
  (let loop ([tok (read-token src in)] [acc '()])
    (cond
      [(eof-object? tok) (reverse acc)]
      [else
       (match-define (followed ts after)
         (indentation-single src in #f tok))
       (loop after (append (group ts) acc))])))

;; wraith-string->sexprs : String -> [Listof S-Expr]
(define (wraith-string->sexprs str)
  (parameterize ([port-count-lines-enabled #true])
    (read-wraith-list (open-input-string str))))

;; tight : Any Input-Port Token -> Tight
(define (tight src in tok)
  (define str (token-string tok))
  (define type (token-type tok))
  (define loc (token-srcloc tok))
  (match type
    ['parenthesis
     (define len (string-length str))
     (define pre (substring str 0 (sub1 len)))
     (define open (substring str (sub1 len)))
     (unless (member open '("(" "[" "}"))
       (read-error (format "unexpected `~a`" open) loc))
     (define close (paren-close open))
     (match pre
       [""
        (match open
          ["(" (handle-parens src in loc open close)]
          ["[" (handle-brackets src in loc open close)]
          ["{" (handle-braces src in loc open close)])]
       ["#" (handle-vector src in loc #f close)]
       ["#s" (handle-prefab src in loc close)]
       ["#hash" (handle-hash src in loc make-immutable-hash close)]
       ["#hasheq" (handle-hash src in loc make-immutable-hasheq close)]
       ["#hasheqv" (handle-hash src in loc make-immutable-hasheqv close)]
       [(regexp #px"#(\\d+)" (list _ d))
        (handle-vector src in loc (string->number d) close)])]
    [_ tok]))

;; paren-close : String -> String
(define (paren-close s)
  (match s
    ["(" ")"]
    ["[" "]"]
    ["{" "}"]))

;; end? : (U Token Eof) (U #f Nat) -> Bool
(define (end? tok lcol)
  (or (eof-object? tok)
      (and (symbol=? (token-type tok) 'parenthesis)
           (member (token-string tok) '(")" "]" "}")))
      (and lcol (<= (srcloc-column (token-srcloc tok)) lcol))))

;; handle-parens : Any Input-Port Srcloc String String -> Syntax
(define (handle-parens src in loc open close)
  (define lcol (srcloc-column loc))
  (define tok (read-token src in))
  (cond
    [(eof-object? tok)
     (read-error (format "expected `~a` to close `~a`" close open) loc)]
    [(string=? (token-string tok) close)
     (datum->syntax #f '()
       (build-source-location-list loc (token-srcloc tok))
       orig-stx)]
    [else
     (define tokloc (token-srcloc tok))
     (unless (< lcol (srcloc-column tokloc))
       (read-error
        (format (string-append
                 "expected `~a` before line ~a to close `~a`\n"
                 "  (assuming indentation is correct)")
                close
                (srcloc-line tokloc)
                open)
        tokloc
        #:extra-srclocs (list loc)))
     (match-define (followed ts after) (indentation-single/tail src in lcol tok))
     (unless (and (token? after) (string=? (token-string after) close))
       (read-error (format "expected `~a` to close `~a`" close open)
                   tokloc
                   #:extra-srclocs (list loc)))
     (match (group ts #t loc (token-srcloc after))
       [(list g) g]
       [l (error 'handle-parens "internal error, not exactly one: ~v" l)])]))

;; handle-brackets : Any Input-Port Srcloc String String -> Syntax
(define (handle-brackets src in loc open close)
  (define lcol (srcloc-column loc))
  (define tok (read-token src in))
  (cond
    [(eof-object? tok)
     (read-error (format "expected `~a` to close `~a`" close open) loc
                 #:extra-srclocs (list (token-srcloc tok)))]
    [(string=? (token-string tok) close)
     (datum->syntax #f '()
       (build-source-location-list loc (token-srcloc tok))
       orig-stx)]
    [else
     (define tokloc (token-srcloc tok))
     (unless (< lcol (srcloc-column tokloc))
       (read-error
        (format (string-append
                 "expected `~a` before line ~a to close `~a`\n"
                 "  (assuming indentation is correct)")
                close
                (srcloc-line tokloc)
                open)
        tokloc
        #:extra-srclocs (list loc)))
     (match-define (followed ts after) (indentation-multiple src in lcol tok))
     (unless (and (token? after) (string=? (token-string after) close))
       (read-error (format "expected `~a` to close `~a`" close open)
                   tokloc
                   #:extra-srclocs (list loc)))
     (match (group ts #t loc (token-srcloc after))
       [(list g) g]
       [l (error 'handle-brackets "internal error, not exactly one: ~v" l)])]))

;; handle-braces : Any Input-Port Srcloc String -> Syntax
(define handle-braces #f)

(define handle-vector #f)
(define handle-prefab #f)
(define handle-hash #f)

;; indentation-single : Any Input-Port (U #f Nat) Token -> [Followed Tights]
(define (indentation-single src in lcol tok)
  (define loc (token-srcloc tok))
  ; line-rev includes tok
  (match-define (followed line-rev after)
    (read-line-reversed src in (srcloc-line loc) lcol tok '()))
  ; (rev line-rev), acc
  (let loop ([line-rev line-rev] [acc '()] [after after])
    (cond
      [(end? after lcol)
       (followed (append-reverse line-rev acc) after)]
      [else
       (define after-col (srcloc-column (token-srcloc after)))
       ; (rev ys), (rev xs), acc
       (define-values [xs ys] (splitf-at line-rev (column>=? after-col)))
       ; each x is right-of/at after-col
       ; each y is left-of after-col
       (match ys
         ; nothing is left-of after-col, this is all
         ['()
          (followed (append-reverse xs acc) after)]
         ; head is the closest thing left-of after-col
         [(list head)
          ; head, (rev xs), acc, tail
          (match-define (followed tail after*)
            (indentation-multiple src in (column head) after))
          (followed
           (cons head (append-reverse xs (append acc tail)))
           after*)]
         [(cons head before)
          ; (rev before), head, (rev xs), acc, tail
          (match-define (followed tail after*)
            (indentation-multiple src in (column head) after))
          (define head-groups
            (group (cons head (append-reverse xs (append acc tail)))))
          (loop before head-groups after*)])])))

;; indentation-multiple : Any Input-Port (U #f Nat) Token -> [Followed Tights]
(define (indentation-multiple src in lcol tok)
  (let loop ([tok tok] [acc '()])
    (match-define (followed ts after) (indentation-single src in lcol tok))
    (define gs (group ts))
    (cond
      [(end? after lcol) (followed (append-reverse acc gs) after)]
      [else              (loop after (append-reverse gs acc))])))

;; indentation-single/tail : Any Input-Port (U #f Nat) Token -> [Followed Tights]
(define (indentation-single/tail src in lcol tok)
  (define s (indentation-single src in lcol tok))
  (match-define (followed ts after) s)
  (cond
    [(end? after lcol) s]
    [(and (cons? ts) (cons? (rest ts))) s]
    [else
     (match-define (followed tail after*) (indentation-multiple src in lcol after))
     (followed (append ts tail) after*)]))

;; read-line-reversed :
;; Any Input-Port Nat (U Nat) (U Token Eof) Tights -> [Followed Tights]
(define (read-line-reversed src in ln lcol tok acc)
  (cond [(end? tok lcol) (followed acc tok)]
        [(string=? (token-string tok) "\\\n")
         (read-line-reversed src in (add1 ln) lcol (read-token src in) acc)]
        [(<= (line tok) ln)
         (define t (tight src in tok))
         (read-line-reversed src in ln lcol (read-token src in) (cons t acc))]
        [else (followed acc tok)]))

(define (tight->syntax t)
  (cond [(syntax? t) t]
        ;; TODO: only for some tokens,
        ;; not special tokens like parens, ticks, and backslash-newlines
        [(token? t)
         (when (symbol=? (token-type t) 'parenthesis)
           (error 'tight->syntax "unexpected paren"))
         (datum->syntax
          #f
          (read (open-input-string (token-string t)))
          (build-source-location-list (token-srcloc t))
          orig-stx)]
        [else (error "expected syntax or a token")]))

;; handle-tick : Symbol Srcloc Tight -> Syntax
(define (handle-tick sym loc t)
  (define loc2 (get-srcloc t))
  (datum->syntax
   #f
   (list (datum->syntax #f sym (build-source-location-list loc))
         (tight->syntax t))
   (build-source-location-list loc loc2)
   orig-stx))

;; handle-ticks : Tights -> Tights
(define (handle-ticks ts)
  (define (next str loc rst)
    (unless (cons? rst)
      (read-error (format "expected something after `~a`" str) loc))
    (first rst))
  (match ts
    ['() '()]
    [(cons (? syntax? stx) rst)
     (cons stx (handle-ticks rst))]
    [(cons (token str 'sexp-comment loc) rst)
     (next str loc rst)
     (handle-ticks (rest rst))]
    [(cons (and t (token str (or 'constant 'other) loc)) rst)
     (match str
       ["'" (cons (handle-tick 'quote loc (next str loc rst))
                  (handle-ticks (rest rst)))]
       ["`" (cons (handle-tick 'quasiquote loc (next str loc rst))
                  (handle-ticks (rest rst)))]
       ["," (cons (handle-tick 'unquote loc (next str loc rst))
                  (handle-ticks (rest rst)))]
       [",@" (cons (handle-tick 'unquote-splicing loc (next str loc rst))
                  (handle-ticks (rest rst)))]
       ["#'" (cons (handle-tick 'syntax loc (next str loc rst))
                   (handle-ticks (rest rst)))]
       ["#`" (cons (handle-tick 'quasisyntax loc (next str loc rst))
                   (handle-ticks (rest rst)))]
       ["#," (cons (handle-tick 'unsyntax loc (next str loc rst))
                   (handle-ticks (rest rst)))]
       ["#,@" (cons (handle-tick 'unsyntax-splicing loc (next str loc rst))
                    (handle-ticks (rest rst)))]
       ["#&" (cons (handle-tick 'box loc (next str loc rst))
                   (handle-ticks (rest rst)))]
       [_ (cons t (handle-ticks rst))])]
    [(cons t rst) (cons t (handle-ticks rst))]))
       

;; group : Tights -> Tights
;; Groups them into a single syntax object,
;; except when the first token is a "non-grouping" token and parens? is #f
(define (group ts
               [parens? #f]
               [loc1 (and (cons? ts) (get-srcloc (first ts)))]
               [loc2 (and (cons? ts) (get-srcloc (last ts)))])
  (match (handle-ticks ts)
    ['()
     (list
      (datum->syntax
       #f
       '()
       (build-source-location-list loc1 loc2)
       orig-stx))]
    [(cons (and t (token _ 'hash-colon-keyword _)) rst)
     (cons t rst)]
    [(list t)
     #:when (not parens?)
     (list (tight->syntax t))]
    [ts
     (list
      (datum->syntax
       #f
       (map tight->syntax ts)
       (build-source-location-list loc1 loc2)
       orig-stx))]))

(module+ test
  (check-equal? (wraith-string->sexprs "a") '(a))
  (check-equal? (wraith-string->sexprs "(a)") '((a)))
  (check-equal? (wraith-string->sexprs "((a))") '(((a))))
  (check-equal? (wraith-string->sexprs "a b c") '((a b c)))
  (check-equal? (wraith-string->sexprs "(a b c)") '((a b c)))
  (check-equal? (wraith-string->sexprs "(a (b c))") '((a (b c))))
  (check-equal? (wraith-string->sexprs
                 "define drawer (make-pict-drawer p)\n")
                '[(define drawer (make-pict-drawer p))])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "define drawer\n"
                  "  make-pict-drawer p\n"))
                '[(define drawer
                    (make-pict-drawer p))])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "define (display-excitement str)\n"
                  "  format \"I'm SO EXCITED about ~a!!!\"\n"
                  "         string-upcase str\n"))
                '[(define (display-excitement str)
                    (format "I'm SO EXCITED about ~a!!!"
                            (string-upcase str)))])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "a b c d e\n"
                  "     f\n"))
                '[(a b (c d e
                         f))])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "(a b c d e\n"
                  "      f)\n"))
                '[(a b (c d e
                         f))])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "a b c d e\n"
                  " f\n"))
                '[(a b c d e
                    f)])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "(a b c d e\n"
                  "  f)\n"))
                '[(a b c d e
                    f)])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "a b c d e\n"
                  "         f\n"))
                '[(a b c d (e
                             f))])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "(a b c d e\n"
                  "          f)\n"))
                '[(a b c d (e
                             f))])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "let [x 5]\n"
                  "  + x 1\n"))
                '[(let [(x 5)]
                    (+ x 1))])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "let [x 5\n"
                  "     y 6]\n"
                  "  + x y\n"))
                '[(let [(x 5)
                        (y 6)]
                    (+ x y))])
  (check-equal? (wraith-string->sexprs
                 (string-append
                  "match '(1 2 3)\n"
                  "  '() 'empty\n"
                  "  '(1) 'single\n"
                  "  '(1 2)\n"
                  "   'double\n"
                  "  _\n"
                  "    'many\n"))
                '[(match '(1 2 3)
                    ('() 'empty)
                    ('(1) 'single)
                    ('(1 2)
                     'double)
                    (_
                     'many))])
  (check-equal? (wraith-string->sexprs #<<```
define (add-drawing p)
  define drawer
    make-pict-drawer p
  new canvas%
      parent f
      style '(border)
      paint-callback
        lambda (self dc)
          drawer dc 0 0
```
                                       )
                '[(define (add-drawing p)
                    (define drawer
                      (make-pict-drawer p))
                    (new canvas%
                         [parent f]
                         [style '(border)]
                         [paint-callback
                          (lambda (self dc)
                            (drawer dc 0 0))]))])
  (check-equal? (wraith-string->sexprs #<<```
define drawer
  make-pict-drawer p
define drawer (make-pict-drawer p)
```
                                       )
                '[(define drawer
                    (make-pict-drawer p))
                  (define drawer (make-pict-drawer p))])
  (check-equal? (wraith-string->sexprs #<<```
define (greet name)
  displayln
    string-append "hello " \
                  name "!"
```
                                       )
                '[(define (greet name)
                    (displayln
                     (string-append "hello "
                                    name "!")))])
  (check-equal? (wraith-string->sexprs #<<```
standard-cat 100 90
             #:happy? #t
standard-cat \
  100 90
  #:happy? #t
```
                                       )
                '[(standard-cat 100 90
                                #:happy? #t)
                  (standard-cat
                    100 90
                    #:happy? #t)])
  (check-equal? (wraith-string->sexprs #<<```
define (display-excitement str)
  format "I'm SO EXCITED about ~a!!!"
         string-upcase str
define (greeter name)
  let ((to-say
          format "Hey there ~a! :D"
                 name))
    displayln to-say
define (greeter name)
  let [to-say
         format "Hey there ~a! :D"
                name]
    displayln to-say
```
                                       )
                '[(define (display-excitement str)
                    (format "I'm SO EXCITED about ~a!!!"
                            (string-upcase str)))
                  (define (greeter name)
                    (let ((to-say
                            (format "Hey there ~a! :D"
                                    name)))
                      (displayln to-say)))
                  (define (greeter name)
                    (let [(to-say
                            (format "Hey there ~a! :D"
                                    name))]
                      (displayln to-say)))])

  ;; with parens "rectangle alignment" with multiple things is an error
  (check-exn #rx"expected `\\)` to close `\\(`"
             (λ ()
               (wraith-string->sexprs #<<```
define a-list '(1 2 3
                4 5 6)
```
                                       )))
  (check-exn #rx"expected `\\)` to close `\\(`"
             (λ ()
               (wraith-string->sexprs #<<```
for/list (x (in-range 0 30 2)
          y (in-naturals))
  * x y
```
                                       )))
  (check-exn #rx"expected `\\)` to close `\\(`"
             (λ ()
               (wraith-string->sexprs #<<```
define n
  sum (map sqr
       range 0 30 2)
```
                                       )))

  (check-equal? (wraith-string->sexprs #<<```
define a-list '(1 2 3 \
                4 5 6)
for/list ((x (in-range 0 30 2))
          (y (in-naturals)))
  * x y
for/list [x (in-range 0 30 2)
          y (in-naturals)]
  * x y
define n
  sum (map
       sqr
       range 0 30 2)
define n
  sum (map sqr
        range 0 30 2)
```
                                       )
                '[(define a-list '(1 2 3
                                   4 5 6))
                  (for/list ((x (in-range 0 30 2))
                             (y (in-naturals)))
                    (* x y))
                  (for/list [(x (in-range 0 30 2))
                             (y (in-naturals))]
                    (* x y))
                  (define n
                    (sum (map
                          sqr
                          (range 0 30 2))))
                  (define n
                    (sum (map sqr
                           (range 0 30 2))))])
  (check-equal? (wraith-string->sexprs #<<```
'(div
   (p (@ (class "cool-paragraph"))
      "Hello everybody! "
      "Here's a picture of my cat: "
      (img (@ (href "cat.jpg")
              (alt "My cat Fluffy")))))
'div
   p (@ (class "cool-paragraph"))
     "Hello everybody! "
     "Here's a picture of my cat: "
     img @ (href "cat.jpg")
           alt "My cat Fluffy"
```
                                       )
                '['(div
                     (p (@ (class "cool-paragraph"))
                        "Hello everybody! "
                        "Here's a picture of my cat: "
                        (img (@ (href "cat.jpg")
                                (alt "My cat Fluffy")))))
                  '(div
                     (p (@ (class "cool-paragraph"))
                        "Hello everybody! "
                        "Here's a picture of my cat: "
                        (img (@ (href "cat.jpg")
                                (alt "My cat Fluffy")))))])
  (check-equal? (wraith-string->sexprs #<<```
for [pet '("cat" "dog" "horse")]
  printf "I love my ~a!\n" pet

define (counting-letters-song letters)
  for [letter letters
       number (in-naturals 1)]
    printf "I like ~a, it's number ~a!" \
      letter number
    (newline)
  displayln "Singing a letters song!"

let* [animal "dog"
      noise "barks"
      player-hears
        format "the ~a says: ~a!!!" \
               animal noise]
  displayln player-hears
```
                                       )
                '[(for [(pet '("cat" "dog" "horse"))]
                    (printf "I love my ~a!\n" pet))
                  (define (counting-letters-song letters)
                    (for [(letter letters)
                          (number (in-naturals 1))]
                      (printf "I like ~a, it's number ~a!"
                              letter number)
                      (newline))
                    (displayln "Singing a letters song!"))
                  (let* [(animal "dog")
                         (noise "barks")
                         (player-hears
                          (format "the ~a says: ~a!!!"
                                  animal noise))]
                    (displayln player-hears))
                  ])

  (check-equal? (wraith-string->sexprs #<<```

```
                                       )
                '[])
  )

