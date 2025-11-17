#lang racket/base
(require shrubbery/print
         shrubbery/syntax-color
         (only-in syntax-color/lexer-contract
                  dont-stop?
                  dont-stop-val)
         syntax/parse/pre
         racket/list
         shrubbery/property
         (for-template "typeset-help.rkt")
         "add-space.rkt"
         "line-shape.rkt")

(provide make)

(define (make
         #:render render ; 'paren, 'meta, 'datum, 'value, 'result, 'comment, or shape from `rendered_shape`
         #:render_in_space render-in-space
         #:render_via_result_annotation [render-via-result-annotation (lambda (root rators field field-str) #f)]
         #:render_whitespace whitespace
         #:render_indentation indentation-whitespace
         #:render_line render-line
         #:render_one_line [render-one-line render-line]
         #:render_lines render-lines
         #:rendered_shape element-shape
         #:is_rendered element*?)
  (define (render_line stx-in
                       #:space [space-name-in #f]
                       #:spacer_info_box [info-box #f]
                       ;; `stx-in` should contain a name (possibly dotted) if `content` is not `#f`
                       #:content [content #f])
    (define tt-space (whitespace 1))
    (define tt-comma (render 'meta ", "))
    (define tt-semicolon (render 'meta "; "))
    (define res-comma (render 'result ", "))
    (define res-semicolon (render 'result "; "))

    (define render-in-space/content
      (if content
          (lambda (space-name
                   #:prefix [prefix-str #f]
                   str
                   id
                   #:suffix [suffix-target #f]
                   #:suffix-space [suffix-space-name #f]
                   #:raw [raw? #f])
            (render-in-space space-name
                             #:prefix #f
                             content
                             id
                             #:suffix suffix-target
                             #:suffix-space suffix-space-name
                             #:raw #t))
          render-in-space))

    (define stx (replace-name-refs (syntax-parse stx-in
                                     #:datum-literals (multi)
                                     [(multi g) #'g]
                                     [(group . _) stx-in]
                                     [_ #`(group #,stx)])
                                   'group info-box
                                   render-in-space/content
                                   (if content
                                       (lambda (root rators field field-str) #f)
                                       render-via-result-annotation)))
    (define space-names (full-space-names space-name-in))
    (define one-space-name (and (pair? space-names) (car space-names)))
    (define (res a [b 'result]) (if (eq? one-space-name 'result) b a))
    ;; "pretty" prints to a single line, currently assuming that the input was
    ;; originally on a single line
    (define (id-space-name* id) (id-space-name id space-names))
    (define elems
      (let loop ([stx stx])
        (define (seq open elems-stx close #:sep [sep (res tt-comma res-comma)])
          (list (render (res 'paren) open)
                (add-between (map loop (syntax->list elems-stx))
                             sep)
                (render (res 'paren) close)))
        (define (group elems-stx)
          (let g-loop ([elems (syntax->list elems-stx)] [pre-space? #f] [check-prefix? #f])
            (cond
              [(null? elems) null]
              [(initial-name-ref elems space-names render-in-space/content) ; detect `root.field` paths
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
                                   [(head . _) (eq? 'space (ends (syntax-raw-suffix-property #'head)))]
                                   [_ #f])
                                 '()))))
               (define add-space?
                 (or pre-space? (prefixed? elem) (and alt-elem (prefixed? alt-elem))))
               (define e (loop (car elems)))
               (cons (cond
                       [add-space? (list tt-space e)]
                       [else e])
                     (if content
                         null
                         (g-loop (cdr elems)
                                 (or (suffixed? elem) (and alt-elem (suffixed? alt-elem)))
                                 #t)))])))
        (cond
          [(element*? (syntax-e stx)) (syntax-e stx)]
          [else
           (syntax-parse stx
             #:datum-literals (multi group parens brackets braces block quotes alts op)
             [(multi g ...)
              (add-between (map loop (syntax->list #'(g ...)))
                           (render (res 'meta) "; "))]
             [(group elem ... (block g ...))
              (list (group #'(elem ...))
                    (render (res 'meta) ": ")
                    (add-between (map loop (syntax->list #'(g ...)))
                                 (render (res 'meta) "; ")))]
             [(group elem ... (alts (block g ...) ...+))
              (list (group #'(elem ...))
                    (render (res 'meta) " | ")
                    (add-between
                     (for/list ([gs (in-list (syntax->list #'((g ...) ...)))])
                       (add-between (map loop (syntax->list gs))
                                    (render (res 'meta) "; ")))
                     (render (res 'meta) " | ")))]
             [(group elem ...)
              (group #'(elem ...))]
             [(parens elem ...) (seq "(" #'(elem ...) ")")]
             [(brackets elem ...) (seq "[" #'(elem ...) "]")]
             [(braces elem ...) (seq "{" #'(elem ...) "}")]
             [(quotes elem ...) (seq "'" #'(elem ...) "'" #:sep (res tt-semicolon res-semicolon))]
             [(op id)
              (define str (or content (string->immutable-string (shrubbery-syntax->string stx))))
              (cond
                [(eq? one-space-name 'datum) (render 'plain str)]
                [(eq? one-space-name 'value) (render 'variable str)]
                [(eq? one-space-name 'result) (render 'result str)]
                [else
                 (define space-name (id-space-name* #'id))
                 (if (identifier-binding (add-space #'id space-name) #f)
                     (render-in-space space-name str (add-space #'id space-name) #:raw (and content #t))
                     (render 'plain str))])]
             [id:identifier
              (define str (or content (string->immutable-string (shrubbery-syntax->string stx))))
              (define space-name (id-space-name* #'id))
              (cond
                [(eq? space-name 'var)
                 (render 'variable str)]
                [(eq? space-name 'datum)
                 (render 'plain str)]
                [(eq? space-name 'value)
                 (render 'value str)]
                [(eq? space-name 'result)
                 (render 'result str)]
                [(identifier-binding (add-space stx space-name) #f)
                 (render-in-space space-name str (add-space stx space-name) #:raw (and content #t))]
                [content content]
                [else
                 (render 'identifier str)])]
             [_
              (define d (syntax->datum stx))
              (render (cond
                        [(eq? one-space-name 'result) 'result]
                        [(symbol? d) 'identifier]
                        [(keyword? d) 'paren]
                        [else 'value])
                      (or content
                          (string->immutable-string (shrubbery-syntax->string stx))))])])))
    (render-one-line elems))

  (define (render_block stx
                        #:indent [indent-amt 0]
                        #:prompt [prompt ""]
                        #:indent_from_block [indent-from-block? #t]
                        #:spacer_info_box [info-box #f]
                        #:number_from [number-from #f])
    ;; Go back to a string, then parse again using the
    ;; colorer. Why didn't we use a string to start with?
    ;; Because having `rhombusblock` work on implicitly quoted syntax
    ;; means that you get nice editor support.
    (define block-stx
      (syntax-parse stx
        #:datum-literals (multi group block)
        [(multi (group (~and b (block . _)))) #'b]
        [(group (~and b (block . _))) #'b]
        [(block . _) stx]
        [_ (error 'typeset-rhombusblock "not a block term: ~e" stx)]))
    (define stx-ranges (make-hasheq))
    (define str (block-string->content-string (shrubbery-syntax->string (replace-name-refs block-stx 'block info-box
                                                                                           render-in-space
                                                                                           render-via-result-annotation)
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
                                                                             ;; write as something self-delimiting:
                                                                             (display "\"elem\"" output)
                                                                             #t]
                                                                            [else #f])))
                                              (syntax-column
                                               (if indent-from-block?
                                                   (syntax-parse block-stx
                                                     #:datum-literals (block)
                                                     [((~and tag block) . _) #'tag])
                                                   (syntax-parse block-stx
                                                     #:datum-literals (group block)
                                                     [(block (group (~or* (a . _) a) . _) . _) #'a])))
                                              (and indent-from-block?
                                                   (syntax-parse block-stx
                                                     #:datum-literals (block)
                                                     [((~and tag block) . _)
                                                      (syntax-raw-property #'tag)]))
                                              indent-from-block?
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
                (cons (whitespace (max 0 (- amt skip-ws)))
                      (ghost-loop start (- skip-ws amt)))]
               [else
                (define type (if (hash? attribs)
                                 (hash-ref attribs 'type 'other)
                                 attribs))
                (define (make-element start end skip-ws line-shape)
                  (define (non-ws e e-len)
                    (values e (line-shape-step-nonws line-shape e e-len element-shape)))
                  (cond
                    [(lookup-stx-typeset start end position-stxes stx-ranges
                                         element*?)
                     => (lambda (e-stx)
                          (define e (syntax-e e-stx))
                          (non-ws e (syntax-span e-stx)))]
                    [(and (or (eq? type 'symbol)
                              (eq? type 'operator))
                          (lookup-stx-identifier start end position-stxes stx-ranges))
                     => (lambda (id)
                          (define str (string->immutable-string (shrubbery-syntax->string id)))
                          (define space-name (id-space-name id (full-space-names #f)))
                          (non-ws
                           (cond
                             [(eq? space-name 'var)
                              (render 'variable str)]
                             [(eq? space-name 'value)
                              (render 'value str)]
                             [(eq? space-name 'result)
                              (render 'result str)]
                             [else
                              (render-in-space space-name str (add-space id space-name))])
                           (- end start)))]
                    [else
                     (define style
                       (case type
                         [(string text constant) 'value]
                         [(symbol) 'identifier]
                         [(parenthesis hash-colon-keyword) 'paren]
                         [(error) 'error]
                         [(comment) 'comment]
                         [(white-space) 'whitespace]
                         [else 'plain]))
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
                       (define m (and (not (eq? style 'whitespace))
                                      (regexp-match-positions #rx"[^ ]" show-str sp-start)))
                       (cond
                         [(= sp-start (string-length show-str))
                          (values null line-shape)]
                         [(or (eq? style 'whitespace)
                              (and m (> (caar m) sp-start)))
                          (define len (- (if m (caar m) (string-length show-str)) sp-start))
                          (define-values (ws-len ws-offset orig-len orig-size ws-style) (line-shape-apply line-shape len))
                          (define e (indentation-whitespace ws-len ws-offset orig-len orig-size ws-style))
                          (let-values ([(es line-shape)
                                        (shape-loop (+ sp-start ws-len)
                                                    (line-shape-step line-shape e ws-len ws-style element-shape))])
                            (values (cons e es) line-shape))]
                         [else
                          (define rest-str (string->immutable-string
                                            (if (= sp-start 0)
                                                show-str
                                                (substring show-str sp-start))))
                          (define e-len (string-length rest-str))
                          (define e (if (eq? style 'whitespace)
                                        (whitespace e-len)
                                        (render style rest-str)))
                          (values (if (eqv? sp-start 0)
                                      e
                                      (list e))
                                  (line-shape-step-nonws line-shape e e-len element-shape))]))]))
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
    (define cleaned-elements+linebreaks
      ;; drop empty lists so that they don't interfere with consecutive-'linebreak detection:
      (for/list ([e (in-list elements+linebreaks)]
                 #:unless (null? e))
        e))
    (define elementss (let loop ([l cleaned-elements+linebreaks])
                        (cond
                          [(null? l) (list null)]
                          [(eq? 'linebreak (car l))
                           (define rl (cdr l))
                           (define r (loop (if (and (pair? rl)
                                                    (eq? 'linebreak (car rl)))
                                               ;; add a space if the line is otherwise empty
                                               (cons (whitespace 1)
                                                     rl)
                                               rl)))
                           (if (null? (car r))
                               r
                               (cons null r))]
                          [else
                           (define r (loop (cdr l)))
                           (cons (cons (car l) (car r))
                                 (cdr r))])))
    (define indent (whitespace indent-amt))
    (define (make-line elements #:first? first? #:i i #:len len)
      (render-line (append
                    (if number-from
                        (list (render 'lineno
                                      (let ([n (+ i number-from)]
                                            [max-n (+ (sub1 len) number-from)])
                                        (define s (number->string n))
                                        (define max-s (number->string max-n))
                                        (string-append-immutable
                                         (make-string (- (string-length max-s) (string-length s)) #\space)
                                         s
                                         " "))))
                        null)
                    (if (zero? indent-amt)
                        elements
                        (cons (cond
                                [first?
                                 (define len (string-length prompt))
                                 (list (whitespace (- indent-amt (min len indent-amt)))
                                       (render 'plain (string->immutable-string
                                                       (substring prompt 0 (min len indent-amt)))))]
                                [else indent])
                              elements)))))
    (cond
      [(null? elementss)
       (render-lines null)]
      [(null? (cdr elementss))
       (render-lines
        (list (make-line (car elementss) #:first? #t #:i 0 #:len 1)))]
      [else
       (render-lines
        (for/list ([elements (in-list elementss)]
                   [i (in-naturals)])
          (make-line elements #:first? (zero? i) #:i i #:len (length elementss))))]))

  (values render_line
          render_block))

(define (block-string->content-string str/crlf col raw-str indent-from-block? stx-ranges)
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
       (values (substring str 1) (+ (or col 0) (if indent-from-block? 1 0)))]
      [(and (not indent-from-block?)
            (regexp-match? #rx"^:" str))
       (shift-stxes! 0 1)
       (values (substring str 1) (or col 0))]
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

(define (lookup-stx-typeset start end position-stxes stx-ranges
                            element*?)
  (lookup-stx (lambda (k) (element*? (syntax-e k)))
              start end position-stxes stx-ranges))

(define (lookup-stx-identifier start end position-stxes stx-ranges)
  (lookup-stx (lambda (stx)
                (and (identifier? stx)
                     (let ([name (id-space-name stx (full-space-names #f))])
                       (or (eq? name 'var)
                           (eq? name 'value)
                           (eq? name 'result)
                           (identifier-binding (add-space stx name) #f)))))
              start end position-stxes stx-ranges))

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

(define (initial-name-ref elems space-names render-in-space)
  (define (dotted-elems? elems)
    (define (two-list? v)
      (and (pair? v)
           (pair? (cdr v))
           (null? (cddr v))))
    (and (identifier? (car elems))
         (pair? (cdr elems))
         (let ([op (syntax->list (cadr elems))])
           (and (two-list? op)
                (eq? (syntax-e (car op)) 'op)
                (eq? (syntax-e (cadr op)) '|.|)))
         (pair? (cddr elems))
         (let ([next (caddr elems)])
           (or (identifier? next)
               (let ([next (syntax->list next)])
                 (and (two-list? next)
                      (eq? (syntax-e (car next)) 'parens)
                      (let ([g (syntax->list (cadr next))])
                        (and (two-list? g)
                             (eq? (syntax-e (car g)) 'group)
                             (let ([op (syntax->list (cadr g))])
                               (and (two-list? op)
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
     (define-values (new-elem leftover)
       (dotted-name-ref (car elems)
                        dotted-elems
                        space-names
                        render-in-space
                        #:parens ptag))
     (cond
       [new-elem
        (define skip (add1 (* 2 (- (length dotted-elems) leftover))))
        (cons new-elem
              (list-tail elems skip))]
       [else #f])]
    [else #f]))

(define (dotted-name-ref head
                         dotted-elems
                         space-names
                         render-in-space
                         #:render-str [render-str #f]
                         #:parens [ptag #f]
                         #:must-all? [must-all? #f])
  (define use-space-names (id-space-name head space-names
                                         #:as-list? #t))
  (define resolved (resolve-name-ref use-space-names
                                     head
                                     dotted-elems
                                     #:parens ptag))
  (define target (and resolved (hash-ref resolved 'target)))
  (cond
    [target
     (define leftover (length (hash-ref resolved 'remains)))
     (define id head)
     (define space-name (hash-ref resolved 'space))
     (define root (hash-ref resolved 'root))
     (values (cond
               [(or (not must-all?) (zero? leftover))
                (datum->syntax target
                               (render-in-space
                                (if root 'rhombus/namespace space-name)
                                #:prefix (hash-ref resolved 'raw-prefix #f)
                                (or render-str
                                    (shrubbery-syntax->string target))
                                (or (and root (add-space root 'rhombus/namespace))
                                    (add-space target space-name))
                                #:suffix (and root target)
                                #:suffix-space (and root space-name))
                               target
                               target)]
               [else target])
             leftover)]
    [else (values #f #f)]))

(struct info-tables (keys       ; key -> stx
                     binds      ; sym -> 'bind value
                     closures)) ; key -> binds
(struct via-result (root-annot
                    rators))

;; replace `root.field` with a typeset element
(define (replace-name-refs stx-pre-keys mode info-box
                           render-in-space
                           render-via-result-annotation)
  (define stx-in (add-keys stx-pre-keys mode))
  (define info (gather-info stx-in mode (if (and (box? info-box)
                                                 (info-tables? (unbox info-box)))
                                            (unbox info-box)
                                            (info-tables #hasheq() #hasheq() #hasheq()))))
  (when (box? info-box) (set-box! info-box info))
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
        [(initial-name-ref elems (full-space-names #f) render-in-space) ; detect `root.field` paths
         => (lambda (new-elems) (loop new-elems))]
        [else
         (cons (replace-in-term (car elems))
               (loop (cdr elems)))])))
  (define (replace-in-term stx)
    (syntax-parse stx
      #:datum-literals (parens brackets braces block quotes multi alts op)
      [((~and tag (~or* parens brackets braces quotes block multi)) g ...)
       (datum->syntax stx (cons #'tag (replace-in-groups #'(g ...))) stx stx)]
      [((~and tag alts) b ...)
       (datum->syntax stx (cons #'tag (replace-in-terms #'(b ...))) stx stx)]
      [_
       (define head (find-namespace-root stx info
                                         #hasheq()))
       (cond
         [(via-result? head)
          (datum->syntax stx (element-via-result head stx render-via-result-annotation) stx stx)]
         [head
          (define-values (new-elem/id leftover)
            (dotted-name-ref head
                             #:render-str (shrubbery-syntax->string stx)
                             (list stx)
                             '(#f)
                             render-in-space
                             #:must-all? #t))
          (cond
            [(and new-elem/id (zero? leftover)) (datum->syntax stx new-elem/id stx stx)]
            [else stx])]
         [stx])]))
  (if (eq? mode 'block)
      (replace-in-term stx-in)
      (replace-in-group stx-in)))

;; ensure that every identifier has a key, so that we can record use information
;; in `gather-info`
(define (add-keys stx-in mode)
  (define (add-in-groups gs)
    (for/list ([g (in-list (syntax->list gs))])
      (add-in-group g)))
  (define (add-in-group g)
    (syntax-parse g
      #:datum-literals (group)
      [((~and tag group) t ...)
       (datum->syntax g (cons #'tag (add-in-terms #'(t ...))) g g)]))
  (define (add-in-terms ts)
    (let loop ([elems (syntax->list ts)])
      (cond
        [(null? elems) null]
        [else
         (cons (add-in-term (car elems))
               (loop (cdr elems)))])))
  (define (add-in-term stx)
    (syntax-parse stx
      #:datum-literals (parens brackets braces block quotes multi alts op)
      [((~and tag (~or* parens brackets braces quotes block multi)) g ...)
       (datum->syntax stx (cons #'tag (add-in-groups #'(g ...))) stx stx)]
      [((~and tag alts) b ...)
       (datum->syntax stx (cons #'tag (add-in-terms #'(b ...))) stx stx)]
      [_
       (if (and (identifier? stx)
                (not (syntax-property stx 'spacer_key)))
           (syntax-property stx 'spacer_key (gensym))
           stx)]))
  (if (eq? mode 'block)
      (add-in-term stx-in)
      (add-in-group stx-in)))

(define (gather-info stx-in mode info)
  (define (gather-in-groups info gs)
    (for/fold ([info info]) ([g (in-list (syntax->list gs))])
      (gather-in-group info g)))
  (define (gather-in-group info g)
    (syntax-parse g
      #:datum-literals (group)
      [((~and tag group) t ...)
       (gather-in-terms info #'(t ...))]))
  (define (gather-in-terms info ts)
    (let loop ([info info] [elems (syntax->list ts)])
      (cond
        [(null? elems) info]
        [else
         (loop (gather-in-term info (car elems))
               (cdr elems))])))
  (define (gather-in-term info stx)
    (syntax-parse stx
      #:datum-literals (parens brackets braces block quotes multi alts op)
      [((~and tag (~or* parens brackets braces quotes block multi)) g ...)
       (gather-in-groups (gather-in-term info #'tag) #'(g ...))]
      [((~and tag alts) b ...)
       (gather-in-terms (gather-in-term info #'tag) #'(b ...))]
      [_
       (define keys (info-tables-keys info))
       (define binds (info-tables-binds info))
       (define closures (info-tables-closures info))
       (define key (syntax-property stx 'spacer_key))
       (define sym (and (identifier? stx)
                        (syntax-e stx)))
       (define bind (and sym (syntax-property stx 'bind)))
       (struct-copy info-tables info
                    [keys (if key
                              (hash-set keys key stx)
                              keys)]
                    [binds (if bind
                               (hash-set binds sym bind)
                               binds)]
                    [closures (if (and sym key)
                                  (hash-set closures key binds)
                                  closures)])]))
  (if (eq? mode 'block)
      (gather-in-term info stx-in)
      (gather-in-group info stx-in)))

(define (find-namespace-root stx info
                             seen
                             #:must-indirect? [must-indirect? #t])
  (define keys (info-tables-keys info))
  (define field (syntax-property stx 'field))
  (case (and field (car field))
    [(of)
     (let of-loop ([val-key (cdr field)] [seen seen])
       (define of-stx (and (not (hash-ref seen val-key #f))
                           (hash-ref keys val-key #f)))
       (define annot (and of-stx (syntax-property (term-ctx of-stx) 'annot)))
       (case (and annot (car annot))
         [(as) ; field of an annotation-as-namespace in the source
          (define id (hash-ref keys (cdr annot) #f))
          ;; use `via-result` to get annotation fallbacks
          (via-result id null)]
         [(as_export) ; field of a specific annotation-as-namespace
          ;; for example, a list construction could declare being an instance of `List`
          (define desc (cdr annot))
          (cond
            [(syntax? desc) (via-result (term-identifier desc) null)]
            [(and (list? desc) (= 4 (length desc)))
             (define-values (mpi sym nom-mpi nom-sym) (apply values desc))
             (syntax-binding-set->syntax (syntax-binding-set-extend
                                          (syntax-binding-set)
                                          sym
                                          #f
                                          mpi
                                          #:nominal-module nom-mpi
                                          #:nominal-symbol nom-sym)
                                         sym)]
            [else #f])]
         [(result) ; check result of some other identifier as function/method
          (define rator (and (not (hash-ref seen (cdr annot) #f))
                             (hash-ref keys (cdr annot) #f)))
          (define ns (and rator
                          (find-namespace-root rator info
                                               (hash-set seen (cdr annot) #t)
                                               #:must-indirect? #f)))
          (cond
            [(via-result? ns)
             ;; chain result lookup
             (via-result (via-result-root-annot ns)
                         (append (via-result-rators ns) (list rator)))]
            [ns
             ;; need to resolve a dotted name relative to an annotation
             (via-result ns (list rator))]
            [(identifier? rator)
             ;; look up result of `rator` via doc info
             (via-result #f (list rator))]
            [else #f])]
         [else
          (cond
            [(identifier? of-stx)
             (define key (syntax-property of-stx 'spacer_key))
             (define binds (and key (hash-ref (info-tables-closures info) key #f)))
             (define binder (and binds (hash-ref binds (syntax-e of-stx) #f)))
             (cond
               [binder
                (case (and (pair? binder) (car binder))
                  [(value)
                   (define next-val-key (cdr binder))
                   (cond
                     [val-key (of-loop next-val-key (hash-set seen val-key #t))]
                     [else #f])]
                  [(as)
                   (define id (hash-ref keys (cdr binder) #f))
                   (cond
                     [id
                      ;; use `via-result` to get annotation fallbacks
                      (via-result id null)]
                     [else #f])]
                  [else #f])]
               [(not must-indirect?)
                of-stx]
               [else #f])]
            [else #f])]))]
    [else #f]))

(define (element-via-result head id render-via-result-annotation)
  (render-via-result-annotation (via-result-root-annot head)
                                (via-result-rators head)
                                id
                                (shrubbery-syntax->string id)))

(define (term-ctx stx)
  (syntax-parse stx
    #:datum-literals (parens brackets braces quotes block alts op)
    [((~and head (~or parens brackets braces quotes block alts op)) . _)
     #'head]
    [else stx]))

(define (term-identifier stx)
  (syntax-parse stx
    #:datum-literals (group op)
    [id:identifier #'id]
    [(group t) (term-identifier #'t)]
    [(op id) #'id]
    [else #f]))

(define (strip-dont-stop state)
  (if (dont-stop? state)
      (dont-stop-val state)
      state))
