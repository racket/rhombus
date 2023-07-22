#lang racket/base
(require shrubbery/print
         shrubbery/syntax-color
         (only-in syntax-color/lexer-contract
                  dont-stop?
                  dont-stop-val)
         scribble/racket
         syntax/parse/pre
         racket/list
         shrubbery/property
         (only-in scribble/core
                  element
                  element?
                  content?
                  paragraph
                  table
                  style
                  plain
                  nested-flow)
         (submod scribble/racket id-element)
         (for-template "typeset-help.rkt")
         "typeset-key-help.rkt"
         "add-space.rkt"
         "line-shape.rkt"
         "hspace.rkt")

(provide typeset-rhombus
         typeset-rhombusblock)

(define (typeset-rhombus stx
                         #:space [space-name-in #f])
  (define space-names (full-space-names space-name-in))
  (define one-space-name (and (pair? space-names) (car space-names)))
  ;; "pretty" prints to a single line, currently assuming that the input was
  ;; originally on a single line
  (define (id-space-name* id) (id-space-name id space-names))
  (let loop ([stx stx])
    (define (seq open elems-stx close #:sep [sep tt-comma])
      (list (element paren-color open)
            (add-between (map loop (syntax->list elems-stx))
                         sep)
            (element paren-color close)))
    (define (group elems-stx)
      (let g-loop ([elems (syntax->list elems-stx)] [pre-space? #f] [check-prefix? #f])
        (cond
          [(null? elems) null]
          [(initial-name-ref elems space-names) ; detect `root.field` paths
           => (lambda (new-elems)
                (g-loop new-elems pre-space? check-prefix?))]
          [else
           (define elem (car elems))
           (define alt-elem (syntax-parse elem
                              #:datum-literals (op)
                              [(op x) #'x]
                              [_ #f]))
           (define (prefixed? stx)
             (and check-prefix?
                  (not (null? (or (syntax-raw-prefix-property stx) '())))))
           (define (ends p)
             (cond
               [(null? p) 'empty]
               [(pair? p)
                (define de (ends (cdr p)))
                (if (eq? de 'empty)
                    (ends (car p))
                    de)]
               [(string? p)
                (cond
                  [(string=? p "") 'empty]
                  [(char-whitespace? (string-ref p (sub1 (string-length p)))) 'space]
                  [else 'nonspace])]
               [else 'empty]))
           (define (suffixed? stx)
             (not (null? (or (eq? 'space (ends (syntax-raw-suffix-property stx)))
                             (syntax-case stx ()
                               [(head . _) (eq? 'space (ends (syntax-raw-tail-property #'head)))]
                               [_ #f])
                             '()))))
           (define add-space?
             (or pre-space? (prefixed? elem) (and alt-elem (prefixed? alt-elem))))
           (define e (loop (car elems)))
           (cons (cond
                   [add-space? (list tt-space e)]
                   [else e])
                 (g-loop (cdr elems)
                         (or (suffixed? elem) (and alt-elem (suffixed? alt-elem)))
                         #t))])))
    (cond
      [(element*? (syntax-e stx)) (syntax-e stx)]
      [else
       (syntax-parse stx
         #:datum-literals (multi group parens brackets braces block quotes alts op)
         [(multi g ...)
          (add-between (map loop (syntax->list #'(g ...)))
                       (element tt-style "; "))]
         [(group elem ... (block g ...))
          (list (group #'(elem ...))
                (element tt-style ": ")
                (add-between (map loop (syntax->list #'(g ...)))
                             (element tt-style "; ")))]
         [(group elem ... (alts (block g ...) ...+))
          (list (group #'(elem ...))
                (element tt-style " | ")
                (add-between
                 (for/list ([gs (in-list (syntax->list #'((g ...) ...)))])
                   (add-between (map loop (syntax->list gs))
                                (element tt-style "; ")))
                 (element tt-style " | ")))]
         [(group elem ...)
          (group #'(elem ...))]
         [(parens elem ...) (seq "(" #'(elem ...) ")")]
         [(brackets elem ...) (seq "[" #'(elem ...) "]")]
         [(braces elem ...) (seq "{" #'(elem ...) "}")]
         [(quotes elem ...) (seq "'" #'(elem ...) "'" #:sep tt-semicolon)]
         [(op id)
          (define str (keep-spaces (shrubbery-syntax->string stx)))
          (cond
            [(eq? one-space-name 'datum) (element tt-style str)]
            [(eq? one-space-name 'value) (element variable-color str)]
            [(eq? one-space-name 'result) (element result-color str)]
            [else
             (define space-name (id-space-name* #'id))
             (if (identifier-binding (add-space #'id space-name) #f)
                 (element tt-style (make-id-element (add-space #'id space-name) str #f
                                                    #:space space-name
                                                    #:unlinked-ok? #t))
                 (element tt-style str))])]
         [id:identifier
          (define str (keep-spaces (shrubbery-syntax->string stx)))
          (define space-name (id-space-name* #'id))
          (cond
            [(eq? space-name 'var)
             (element variable-color str)]
            [(eq? space-name 'datum)
             (element tt-style str)]
            [(eq? space-name 'value)
             (element value-color str)]
            [(eq? space-name 'result)
             (element result-color str)]
            [(identifier-binding (add-space stx space-name) #f)
             (element tt-style (make-id-element (add-space stx space-name) str #f
                                                #:space space-name
                                                #:unlinked-ok? #t))]
            [else
             (element symbol-color str)])]
         [_
          (define d (syntax->datum stx))
          (element (cond
                     [(symbol? d) symbol-color]
                     [(keyword? d) paren-color]
                     [else value-color])
            (keep-spaces (shrubbery-syntax->string stx)))])])))

(define (typeset-rhombusblock stx
                              #:inset [inset? #t]
                              #:indent [indent-amt 0]
                              #:prompt [prompt ""])
  ;; Go back to a string, then parse again using the
  ;; colorer. Why didn't we use a string to start with?
  ;; Because having `rhombusblock` work on implicitly quoted syntax
  ;; means that you get nice editor support.
  (define block-stx
    (syntax-parse stx
      #:datum-literals (muti group block)
      [(multi (group (~and b (block . _)))) #'b]
      [(group (~and b (block . _))) #'b]
      [(block . _) stx]
      [else (error 'typeset-rhombusblock "not a block term: ~e" stx)]))
  (define stx-ranges (make-hasheq))
  (define str (block-string->content-string (shrubbery-syntax->string (replace-name-refs block-stx)
                                                                      #:use-raw? #t
                                                                      #:keep-suffix? #t
                                                                      #:infer-starting-indentation? #f
                                                                      #:register-stx-range
                                                                      (lambda (stx start end)
                                                                        (hash-set! stx-ranges stx (cons start end)))
                                                                      #:render-stx-hook
                                                                      (lambda (stx output)
                                                                        (cond
                                                                          [(element*? (syntax-e stx))
                                                                           (display "ELEM" output)
                                                                           #t]
                                                                          [else #f])))
                                            (syntax-case block-stx ()
                                              [(b . _) (syntax-column #'b)])
                                            (syntax-case block-stx ()
                                              [(b . _) (syntax-raw-property #'b)])
                                            stx-ranges))
  (define position-stxes (for/fold ([ht #hasheqv()]) ([(k v) (in-hash stx-ranges)])
                           (hash-set ht (car v) (cons k (hash-ref ht (car v) '())))))
  (define init-col (infer-indentation str))
  (define in (open-input-string str))
  (port-count-lines! in)
  (define elements+linebreaks
    ;; loop over all tokens in `str`
    (let token-loop ([state #f]            ; lexer state
                     [pos 0]               ; number of positions typeset so far
                     [skip-ws init-col]    ; amount of leading space on this line to be skipped
                     [line-shape (make-line-shape)]) ; for generating compatible (e.g., bold) leading whitespace
      (define-values (lexeme attribs paren start+1 end+1 backup new-state)
        (shrubbery-lexer in 0 (strip-dont-stop state)))
      (cond
        [(eof-object? lexeme) null]
        [else
         (define start (sub1 start+1))
         (define end (sub1 end+1))
         ;; `ghost-loop` deals with content (treated as space) that
         ;; somehow doesn't show up as a token; it runs only once or twice
         (let ghost-loop ([pos pos] [skip-ws skip-ws])
           (cond
             [(pos . < . start)
              (define amt (- start pos))
              (cons (element hspace-style (make-string (max 0 (- amt skip-ws)) #\space))
                    (ghost-loop start (- skip-ws amt)))]
             [else
              (define type (if (hash? attribs)
                               (hash-ref attribs 'type 'other)
                               attribs))
              (define (make-element start end skip-ws line-shape)
                (define (non-ws e) (values e (line-shape-step-nonws line-shape e)))
                (cond
                  [(lookup-stx-typeset start end position-stxes stx-ranges)
                   => (lambda (e)
                        (non-ws e))]
                  [(and (or (eq? type 'symbol)
                            (eq? type 'operator))
                        (lookup-stx-identifier start end position-stxes stx-ranges))
                   => (lambda (id)
                        (define str (shrubbery-syntax->string id))
                        (define space-name (id-space-name id (full-space-names #f)))
                        (non-ws
                         (cond
                           [(eq? space-name 'var)
                            (element variable-color str)]
                           [(eq? space-name 'value)
                            (element value-color str)]
                           [(eq? space-name 'result)
                            (element result-color str)]
                           [else
                            (element tt-style (make-id-element (add-space id space-name) str #f
                                                               #:space space-name
                                                               #:unlinked-ok? #t))])))]
                  [else
                   (define style
                     (case type
                       [(string text constant) value-color]
                       [(symbol) symbol-color]
                       [(parenthesis hash-colon-keyword) paren-color]
                       [(error) error-color]
                       [(comment) comment-color]
                       [(white-space) hspace-style]
                       [else tt-style]))
                   (define show-str
                     (substring str
                                (let loop ([start start] [skip-ws skip-ws])
                                  (cond
                                    [(skip-ws . <= . 0) start]
                                    [(start . >= . end) start]
                                    [(char=? #\space (string-ref str start))
                                     (loop (add1 start) (sub1 skip-ws))]
                                    [else start]))
                                end))
                   ;; `shape-loop` handles whitespace shaping as needed to match the first
                   ;; line, which might have bold text so that bold hspace is needed to
                   ;; preserve spacing
                   (let shape-loop ([sp-start 0] [line-shape line-shape])
                     (define m (and (not (eq? style hspace-style))
                                    (regexp-match-positions #rx"[^ ]" show-str sp-start)))
                     (cond
                       [(= sp-start (string-length show-str))
                        (values null line-shape)]
                       [(or (eq? style hspace-style)
                            (and m (> (caar m) sp-start)))
                        (define len (- (if m (caar m) (string-length show-str)) sp-start))
                        (define-values (ws-len ws-style) (line-shape-apply line-shape len))
                        (define e (if (eq? ws-style 'target)
                                      ;; make whitespace use the same style as a defined name
                                      (element 'tt (element value-def-color
                                                     (for/list ([i (in-range ws-len)]) 'nbsp)))
                                      ;; normal whitespace
                                      (element hspace-style (make-string ws-len #\space))))
                        (let-values ([(es line-shape)
                                      (shape-loop (+ sp-start ws-len) (line-shape-step line-shape e))])
                          (values (cons e es) line-shape))]
                       [else
                        (define e (element style (if (eq? style 'white-space)
                                                     show-str
                                                     (keep-spaces show-str))))
                        (values (if (eqv? sp-start 0)
                                    e
                                    (list e))
                                (line-shape-step-nonws line-shape e))]))]))
              ;; `one-token-loop` eventually emits the token, but handles newlines;
              ;; most newlines are separate whitespace tokens, but they are also
              ;; potentially in the middle of a whitespace or comment token
              (let one-token-loop ([start start] [end end] [skip-ws skip-ws] [line-shape line-shape])
                (define nl (regexp-match-positions #rx"\n" str start end))
                (cond
                  [nl
                   ;; there's a newline inside the token
                   (cond
                     [(= (caar nl) start)
                      ;; the newline is at the start of the token
                      (cons 'linebreak
                            (let ([line-shape (line-shape-newline line-shape)])
                              (if (= (cdar nl) end)
                                  ;; the newline is the whole token, so move to one next
                                  ;; token, which is the next line
                                  (token-loop new-state end init-col line-shape)
                                  ;; there's more in the token after the newline
                                  (one-token-loop (add1 start) end init-col line-shape))))]
                     [else
                      ;; the newline is not at the start of the token, so emit
                      ;; a partial token
                      (define upto (caar nl))
                      (define-values (e new-line-shape) (make-element start upto skip-ws line-shape))
                      (cons e
                            (one-token-loop upto end (- skip-ws (- upto start)) new-line-shape))])]
                  [else
                   ;; no newline within the token
                   (define-values (e new-line-shape) (make-element start end skip-ws line-shape))
                   (cons e
                         (token-loop new-state end (- skip-ws (- end start)) new-line-shape))]))]))])))
  (define elementss (let loop ([l elements+linebreaks])
                      (cond
                        [(null? l) (list null)]
                        [(eq? 'linebreak (car l))
                         (define rl (cdr l))
                         (define r (loop (if (and (pair? rl)
                                                  (eq? 'linebreak (car rl)))
                                             (cons (element hspace-style " ")
                                                   rl)
                                             rl)))
                         (if (null? (car r))
                             r
                             (cons null r))]
                        [else
                         (define r (loop (cdr l)))
                         (cons (cons (car l) (car r))
                               (cdr r))])))
  (define indent (element hspace-style (make-string indent-amt #\space)))
  (define (make-line elements #:first? first?)
    (paragraph plain (if (zero? indent-amt)
                         elements
                         (cons (cond
                                 [first?
                                  (define len (string-length prompt))
                                  (list (element hspace-style
                                          (make-string (- indent-amt (min len indent-amt)) #\space))
                                        (element tt-style (substring prompt 0 (min len indent-amt))))]
                                 [else indent])
                               elements))))
  (define output-block
    (cond
      [(null? elementss)
       (element plain "")]
      [(null? (cdr elementss))
       (make-line (car elementss) #:first? #t)]
      [else
       (table plain
              (for/list ([elements (in-list elementss)]
                         [i (in-naturals)])
                (list (make-line elements #:first? (zero? i)))))]))
  (if inset?
      (nested-flow (style 'code-inset null) (list output-block))
      output-block))
  
(define tt-style (style 'tt null))

(define tt-space (element tt-style " "))
(define tt-comma (element tt-style ", "))
(define tt-semicolon (element tt-style "; "))

(define (block-string->content-string str/crlf col raw-str stx-ranges)
  (define str (regexp-replace* #rx"\r\n" str/crlf "\n"))
  ;; strip `:` from the beginning, add spaces
  ;; corresponding to `col`, then strip any blank newlines
  (define (shift-stxes! after delta)
    ;; shift earlier by `delta`
    (unless (zero? delta)
      (for ([(k v) (in-hash stx-ranges)])
        (when ((car v) . > . after)
          (hash-set! stx-ranges k (cons (- (car v) delta) (- (cdr v) delta)))))))
  (define-values (content-str prefix-len)
    (cond
      [(regexp-match-positions #rx"^:«(.*)»[ ]*$" str)
       => (lambda (m)
            (define delta (caadr m))
            (shift-stxes! 0 delta)
            (values (substring str delta (cdadr m))
                    (+ (or col 0) 2)))]
      [(regexp-match? (if (equal? raw-str "")
                          #rx"^\n"
                          #rx"^[:\n]")
                      str)
       (shift-stxes! 0 1)
       (values (substring str 1) (+ (or col 0) 1))]
      [else
       (values str (or col 0))]))
  (shift-stxes! -1 (- prefix-len))
  (define full-str
    (string-append (make-string prefix-len #\space) content-str))
  (define strip-pre-str (regexp-replace* #px"^\\s*\n" full-str ""))
  (shift-stxes! -1 (- (string-length full-str) (string-length strip-pre-str)))
  (regexp-replace* #px"\\s*\n\\s*$" strip-pre-str ""))

(define (infer-indentation str)
  (define m (regexp-match-positions #px"[^ ]" str))
  (if m
      (caar m)
      0))

(define (lookup-stx keep? start end position-stxes stx-ranges)
  (define stxes (hash-ref position-stxes start '()))
  (for/or ([k (in-list stxes)])
    (define p (hash-ref stx-ranges k))
    (and (= (cdr p) end)
         (cond
           [(keep? k) k]
           [else #f]))))

(define (lookup-stx-typeset start end position-stxes stx-ranges)
  (define k (lookup-stx (lambda (k) (element*? (syntax-e k)))
                         start end position-stxes stx-ranges))
  (and k (syntax-e k)))

(define (lookup-stx-identifier start end position-stxes stx-ranges)
  (lookup-stx (lambda (stx)
                (and (identifier? stx)
                     (let ([name (id-space-name stx (full-space-names #f))])
                       (or (eq? name 'var)
                           (eq? name 'value)
                           (eq? name 'result)
                           (identifier-binding (add-space stx name) #f)))))
              start end position-stxes stx-ranges))

(define (element*? v)
  (and (not (null? v))
       (not (string? v))
       (not (symbol? v))
       (content? v)))

(define (id-space-name id space-names #:as-list? [as-list? #f])
  (let* ([prop (syntax-property id 'typeset-space-name)]
         [space-names (if prop
                          (full-space-names prop)
                          space-names)])
    (cond
      [as-list? space-names]
      [(and (pair? space-names)
            (null? (cdr space-names)))
       (car space-names)]
      [else
       (define space-name* (for/or ([space-name (in-list space-names)])
                             (and (identifier-distinct-binding (add-space id space-name)
                                                               id
                                                               #f)
                                  ;; note; `space-name` might be #f
                                  (or space-name #t))))
       (if space-name*
           (if (eq? space-name* #t) #f space-name*)
           (last space-names))])))

(define (initial-name-ref elems space-names)
  (define (dotted-elems? elems)
    (and (identifier? (car elems))
         (pair? (cdr elems))
         (let ([op (syntax->list (cadr elems))])
           (and op
                (= (length op) 2)
                (eq? (syntax-e (car op)) 'op)
                (eq? (syntax-e (cadr op)) '|.|)))
         (pair? (cddr elems))
         (let ([next (caddr elems)])
           (or (identifier? next)
               (let ([next (syntax->list next)])
                 (and next
                      (= (length next) 2)
                      (eq? (syntax-e (car next)) 'parens)
                      (let ([g (syntax->list (cadr next))])
                        (and g
                             (= (length g) 2)
                             (eq? (syntax-e (car g)) 'group)
                             (let ([op (syntax->list (cadr g))])
                               (and op
                                    (= (length op) 2)
                                    (eq? (syntax-e (car op)) 'op)))))))))))
  (define (extract-op a)
    (cadr (syntax->list (cadr (syntax->list (cadr (syntax->list a)))))))
  (define (extract-ptag a)
    (car (syntax->list a)))
  (cond
    [(dotted-elems? elems)
     (define-values (dotted-elems ptag)
       (let loop ([elems (cddr elems)])
         (cond
           [(dotted-elems? elems)
            (define-values (ds ptag) (loop (cddr elems)))
            (values (cons (car elems) ds) ptag)]
           [else
            (define a (car elems))
            (if (identifier? a)
                (values (list a) #f)
                (values (list (extract-op a))
                        (extract-ptag a)))])))
     (define use-space-names (id-space-name (car elems) space-names
                                            #:as-list? #t))
     (define target+rest+space (resolve-name-ref use-space-names
                                                 (if (pair? (cdr elems))
                                                     (add-space (car elems)
                                                                'rhombus/namespace)
                                                     (car elems))
                                                 dotted-elems
                                                 #:parens ptag))
     (define target (and target+rest+space (car target+rest+space)))
     (cond
       [target
        (define skip (add1 (* 2 (- (length dotted-elems) (length (cadr target+rest+space))))))
        (define id (car elems))
        (define space-name (caddr target+rest+space))
        (cons (datum->syntax target
                             (element tt-style
                               (make-id-element (add-space id (if (pair? (cdr elems))
                                                                  'rhombus/namespace
                                                                  space-name))
                                                (shrubbery-syntax->string target)
                                                #f
                                                #:space 'rhombus/namespace
                                                #:suffix (list (target-id-key-symbol target)
                                                               space-name)
                                                #:unlinked-ok? #t))
                             target
                             target)
              (list-tail elems skip))]
       [else #f])]
    [else #f]))

;; replace `root.field` with a typeset element
(define (replace-name-refs block-stx)
  (define (replace-in-groups gs)
    (for/list ([g (in-list (syntax->list gs))])
      (replace-in-group g)))
  (define (replace-in-group g)
    (syntax-parse g
      #:datum-literals (group)
      [((~and tag group) t ...)
       (datum->syntax g (cons #'tag (replace-in-terms #'(t ...))) g g)]))
  (define (replace-in-terms ts)
    (let loop ([elems (syntax->list ts)])
      (cond
        [(null? elems) null]
        [(initial-name-ref elems (full-space-names #f)) ; detect `root.field` paths
         => (lambda (new-elems) (loop new-elems))]
        [else
         (cons (replace-in-term (car elems))
               (loop (cdr elems)))])))
  (define (replace-in-term stx)
    (syntax-parse stx
      #:datum-literals (parens brackets braces block quotes multi alts op)
      [((~and tag (~or parens brackets braces quotes block multi)) g ...)
       (datum->syntax stx (cons #'tag (replace-in-groups #'(g ...))) stx stx)]
      [((~and tag alts) b ...)
       (datum->syntax stx (cons #'tag (replace-in-terms #'(b ...))) stx stx)]
      [else stx]))
  (replace-in-term block-stx))

(define (strip-dont-stop state)
  (if (dont-stop? state)
      (dont-stop-val state)
      state))
