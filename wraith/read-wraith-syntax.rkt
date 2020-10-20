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
         (indentation-single src in tok))
       (loop after (cons (group ts) acc))])))

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
          ["(" (handle-parens src in loc close)]
          ["[" (handle-brackets src in loc close)]
          ["{" (handle-braces src in loc close)])]
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

;; end? : (U Token Eof) -> Bool
(define (end? tok)
  (or (eof-object? tok)
      (and (symbol=? (token-type tok) 'parenthesis)
           (member (token-string tok) '(")" "]" "}")))))

;; handle-parens : Any Input-Port Srcloc String -> Syntax
(define (handle-parens src in loc close)
  (define tok (read-token src in))
  (cond
    [(eof-object? tok)
     (read-error (format "expected `~a` to close" close) loc)]
    [(string=? (token-string tok) close)
     (datum->syntax #f '()
       (build-source-location-list loc (token-srcloc tok)))]
    [else
     (define tokloc (token-srcloc tok))
     (unless (< (srcloc-column loc) (srcloc-column tokloc))
       (read-error
        (format "expected `~a` before line ~a (assuming indentation is correct)"
                close
                (srcloc-line tokloc))
        tokloc
        #:extra-srclocs (list loc)))
     (match-define (followed ts after) (indentation-single src in tok))
     (unless (and (token? after) (string=? (token-string after) close))
       (read-error (format "expected `~a` to close" close)
                   tokloc
                   #:extra-srclocs (list loc)))
     (group ts #t loc (token-srcloc after))]))

;; handle-brackets : Any Input-Port Srcloc String -> Syntax
(define (handle-brackets src in loc close)
  (define tok (read-token src in))
  (cond
    [(eof-object? tok)
     (read-error (format "expected `~a` to close" close) loc)]
    [(string=? (token-string tok) close)
     (datum->syntax #f '()
       (build-source-location-list loc (token-srcloc tok)))]
    [else
     (define tokloc (token-srcloc tok))
     (unless (< (srcloc-column loc) (srcloc-column tokloc))
       (read-error
        (format "expected `~a` before line ~a (assuming indentation is correct)"
                close
                (srcloc-line tokloc))
        tokloc
        #:extra-srclocs (list loc)))
     (match-define (followed ts after) (indentation-multiple src in tok))
     (unless (and (token? after) (string=? (token-string after) close))
       (read-error (format "expected `~a` to close" close)
                   tokloc
                   #:extra-srclocs (list loc)))
     (group ts #t loc (token-srcloc after))]))

;; handle-braces : Any Input-Port Srcloc String -> Syntax
(define handle-braces #f)

(define handle-vector #f)
(define handle-prefab #f)
(define handle-hash #f)

;; indentation-single : Any Input-Port Token -> [Followed Tights]
(define (indentation-single src in tok)
  (define loc (token-srcloc tok))
  ; line-rev includes tok
  (match-define (followed line-rev after)
    (read-line-reversed src in (srcloc-line loc) tok '()))
  ; (rev line-rev), acc
  (let loop ([line-rev line-rev] [acc '()] [after after])
    (cond
      [(end? after)
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
            (indentation-multiple src in after))
          (followed
           (cons head (append-reverse xs (append acc tail)))
           after*)]
         [(cons head before)
          ; (rev before), head, (rev xs), acc, tail
          (match-define (followed tail after*)
            (indentation-multiple src in after))
          (define head-group
            (group (cons head (append-reverse xs (append acc tail)))))
          (loop before (list head-group) after*)])])))

;; indentation-multiple : Any Input-Port Token -> [Followed [Listof Syntax]]
(define (indentation-multiple src in tok)
  (define loc (token-srcloc tok))
  (define col (srcloc-column loc))
  (let loop ([tok tok] [acc '()])
    (match-define (followed ts after) (indentation-single src in tok))
    (define g (group ts))
    (cond
      [(end? after)           (followed (reverse (cons g acc)) after)]
      [(< (column after) col) (followed (reverse (cons g acc)) after)]
      [else
       (unless (= col (column after))
         (error 'read-wraith "internal error"))
       (loop after (cons g acc))])))

;; read-line-reversed : Any Input-Port Nat (U Token Eof) Tights -> [Followed Tights]
(define (read-line-reversed src in ln tok acc)
  (cond [(end? tok) (followed acc tok)]
        [(<= (line tok) ln)
         (define t (tight src in tok))
         (read-line-reversed src in ln (read-token src in) (cons t acc))]
        [else (followed acc tok)]))

(define (tight->syntax t)
  (cond [(syntax? t) t]
        ;; TODO: only for some tokens,
        ;; not special tokens like parens, dots, and ticks
        [(token? t)
         (when (symbol=? (token-type t) 'parenthesis)
           (error 'tight->syntax "unexpected paren"))
         (datum->syntax
          #f
          (read (open-input-string (token-string t)))
          (build-source-location-list (token-srcloc t)))]
        [else (error "expected syntax or a token")]))

;; handle-tick : Symbol Srcloc Tight -> Syntax
(define (handle-tick sym loc t)
  (define loc2 (get-srcloc t))
  (datum->syntax
   #f
   (list (datum->syntax #f sym (build-source-location-list loc))
         (tight->syntax t))
   (build-source-location-list loc loc2)))

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
       

;; group : Tights -> Syntax
(define (group ts
               [parens? #f]
               [loc1 (and (cons? ts) (get-srcloc (first ts)))]
               [loc2 (and (cons? ts) (get-srcloc (last ts)))])
  (match (handle-ticks ts)
    ['() (datum->syntax #f '() (build-source-location-list loc1 loc2))]
    [(list t) #:when (not parens?) (tight->syntax t)]
    [ts
     (datum->syntax
      #f
      (map tight->syntax ts)
      (build-source-location-list loc1 loc2))]))

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

  (check-equal? (wraith-string->sexprs #<<```

```
                                       )
                '[])
  )

