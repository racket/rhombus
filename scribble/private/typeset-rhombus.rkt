#lang racket/base
(require shrubbery/print
         shrubbery/syntax-color
         scribble/racket
         (only-in scribble/core
                  element
                  paragraph
                  table
                  style
                  plain))

(provide typeset-rhombus
         typeset-rhombusblock)

(define (typeset-rhombus stx)
  (define content-stx
    (syntax-case stx ()
      [(_ group) #'group]))
  (define str (shrubbery-syntax->string content-stx))
  (element tt-style str))

(define (typeset-rhombusblock stx)
  ;; Go back to a string, then parse again using the
  ;; colorer. Why didn't we use a string to start with?
  ;; Because having `rhm` work on implicitly quoted syntax
  ;; means that you get nice editor support.
  (define content-stx
    (syntax-case stx ()
      [(_ (_ (block group ...))) #'(group ...)]))
  (define str (shrubbery-syntax->string content-stx))
  (define init-col (or (syntax-case content-stx ()
                         [((_ a . _) . _) (syntax-column #'a)]
                         [else #f])
                       0))
  (define in (open-input-string str))
  (port-count-lines! in)
  (define elements+linebreaks
    (let loop ([state #f] [pos 0] [skip-ws 0])
      (define-values (lexeme attribs paren start+1 end+1 backup new-state)
        (shrubbery-lexer in 0 state))
      (cond
        [(eof-object? lexeme) null]
        [else
         (define start (sub1 start+1))
         (define end (sub1 end+1))
         (let t-loop ([pos pos] [skip-ws skip-ws])
           (cond
             [(pos . < . start)
              (define amt (- start pos))
              (cons (element hspace-style (make-string (max 0 (- amt skip-ws)) #\space))
                    (t-loop start (- skip-ws amt)))]
             [else
              (define type (if (hash? attribs)
                               (hash-ref attribs 'type 'other)
                               attribs))
              (define (make-element start end skip-ws)
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
                (define m (and (not (eq? style hspace-style))
                               (regexp-match-positions #rx"[^ ]" show-str)))
                (cond
                  [(and m (positive? (caar m)))
                   (list
                    (element hspace-style (make-string (caar m) #\space))
                    (element style (substring show-str (caar m))))]
                  [else
                   (element style show-str)]))
              (let token-loop ([start start] [end end] [skip-ws skip-ws])
                (define nl (regexp-match-positions #rx"\n" str start end))
                (cond
                  [nl
                   (cond
                     [(= (caar nl) start)
                      (cons 'linebreak
                            (if (= (cdar nl) end)
                                (loop state end init-col)
                                (token-loop (add1 start) end init-col)))]
                     [else
                      (define upto (caar nl))
                      (cons (make-element start upto skip-ws)
                            (token-loop upto end (- skip-ws (- upto start))))])]
                  [else
                   (cons (make-element start end skip-ws)
                         (loop new-state end (- skip-ws (- end start))))]))]))])))
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
  (define indent (element hspace-style "  "))
  (define (make-line elements)
    (paragraph plain (cons indent elements)))
  (cond
    [(null? elementss)
     (element plain "")]
    [(null? (cdr elementss))
     (make-line (car elementss))]
    [else
     (table plain
            (map (lambda (elements)
                   (list (make-line elements)))
                 elementss))]))

(define tt-style (style 'tt null))
(define hspace-style (style 'hspace null))
