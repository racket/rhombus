#lang racket/base
(require racket/class
         "private/edit-help.rkt"
         (only-in "parse.rkt" parse-all))

(provide toggle-armor
         armor-region
         unarmor-region)

(struct parse-state (gs skip? prev))
(struct group (col
               bar-line  ; line start for a `|`
               add-semi?
               close?
               in-at?)
  #:transparent)

(define (make-parse-state gs
                          #:skip? [skip? #f]
                          #:prev [prev #f])
  (parse-state gs
               skip?
               prev))

(define (make-group col
                    #:bar-line [bar-line #f]
                    #:add-semi? [add-semi? #t]
                    #:close? [close? #t]
                    #:in-at? [in-at? #f])
  (group col bar-line add-semi? close? in-at?))

(define (toggle-armor t e)
  (define orig-from-pos (send t get-start-position))
  (define orig-to-pos (send t get-end-position))
  (define post-lang-from-pos (skip-hash-lang t orig-from-pos))
  (define from-pos (start-of-group t post-lang-from-pos (line-start t post-lang-from-pos)))
  (define last-pos (send t last-position))
  (define to-pos (let loop ([end from-pos])
                   (define-values (s e) (skip-whitespace t end 1))
                   (cond
                     [(or (and (s . > . orig-to-pos)
                               ((send t position-paragraph s) . > . (send t position-paragraph orig-to-pos)))
                          (end . >= . last-pos)
                          (eq? 'whitespace (classify-position t s)))
                      end]
                     [else
                      (define next (end-of-current t s #:stop-at-comma? #t))
                      (cond
                        [(eqv? next end) end]
                        [else (loop next)])])))
  (define openers '(";«" "|«"))
  (define closer "»")
  (cond
    [(and (from-pos . >= . 2)
          (to-pos . <= . (- last-pos 1))
          (member (send t get-text (- from-pos 2) from-pos) openers)
          (equal? (send t get-text to-pos (+ to-pos 1)) closer))
     (unarmor-region t (- from-pos 2) (+ to-pos 1))]
    [(and (from-pos . <= . (- to-pos 3))
          (member (send t get-text from-pos (+ from-pos 2)) openers)
          (equal? (send t get-text (- to-pos 1) to-pos) closer))
     (unarmor-region t from-pos to-pos)]
    [else
     (armor-region t from-pos to-pos)]))

(define (armor-region t from-pos to-pos)
  (define end to-pos)
  (define start (line-start t from-pos))
  (define col (- from-pos start))
  (cond
    [(not (parse-text t col from-pos end))
     (void)]
    [else
     (define bar? (equal? "|" (send t get-text from-pos (add1 from-pos))))
     (define end-maybe-line (send t position-paragraph end))
     (define end-line (if (= (send t paragraph-start-position end-maybe-line) end)
                          (sub1 end-maybe-line)
                          end-maybe-line))
     (define skip-adjust-lines (make-hasheqv))
     (define init from-pos)
     (define gs (list (make-group col #:close? (not bar?))))
     (define inserts
       (let loop ([pos init] [prev-end init] [init? #t] [state (make-parse-state gs)] [inserts (if bar?
                                                                                                   null
                                                                                                   (list (cons init 'enter)))])
         (define (close-one)
           (loop pos prev-end #f
                 (struct-copy parse-state state
                              [gs (cdr (parse-state-gs state))])
                 (if (group-close? (car (parse-state-gs state)))
                     (cons (cons prev-end 'close) inserts)
                     inserts)))
         (cond
           [(pos . >= . end)
            (cond
              [(pair? (parse-state-gs state)) (close-one)]
              [else inserts])]
           [else
            (define-values (s e) (send t get-token-range pos))
            (define category (classify-position t pos))
            (case category
              [(whitespace comment continue-operator)
               (when (and (eq? category 'comment)
                          (pair? (parse-state-gs state))
                          (group-in-at? (car (parse-state-gs state))))
                 ;; line comment => don't adjust next line's indentation
                 (hash-set! skip-adjust-lines (add1 (send t position-paragraph s)) #t))
               (loop e prev-end init? state inserts)]
              [(group-comment)
               (define next-pos (skip-whitespace2 t e end))
               (loop next-pos prev-end #t state (if (and (not init?)
                                                         (pair? (parse-state-gs state))
                                                         (group-add-semi? (car (parse-state-gs state)))
                                                         (not (eq? 'bar-operator (classify-position t next-pos))))
                                                    (cons (cons prev-end 'semi) inserts)
                                                    inserts))]
              [(closer at-closer)
               (cond
                 [(pair? (parse-state-gs state))
                  (close-one)]
                 [else (loop e e #f
                             (or (parse-state-prev state) state)
                             inserts)])]
              [(comma-operator)
               (cond
                 [(and (pair? (parse-state-gs state))
                       (or (not (parse-state-prev state))
                           (pair? (cdr (parse-state-gs state)))))
                  (close-one)]
                 [else
                  (loop e e #f state inserts)])]
              [else
               (define line (line-start t s))
               (define col (+ (- s line) (line-delta t line)))
               (define g (and (pair? (parse-state-gs state))
                              (car (parse-state-gs state))))
               (define g-col (and (not (parse-state-skip? state))
                                  g
                                  (group-col g)))
               (define (more-nested)
                 (define use-category (cond
                                        [(parse-state-skip? state) (case category
                                                                     [(opener at-opener) category]
                                                                     [else 'skipped])]
                                        [else category]))
                 (case use-category
                   [(block-operator bar-operator)
                    (define next-pos (skip-whitespace2 t e end #:same-line? #t))
                    (define (close-immediately next-pos)
                      (loop next-pos e #f state (list* (cons e 'close)
                                                       (cons e 'open)
                                                       inserts)))
                    (cond
                      [(next-pos . >= . end)
                       (close-immediately next-pos)]
                      [else
                       (define category (classify-position t next-pos))
                       (cond
                         [(armor-opener? t next-pos)
                          ;; assume already armored
                          (loop next-pos e #f state inserts)]
                         [else
                          (define init-pos (skip-whitespace2 t next-pos end #:and-own-line-group-comment next-pos))
                          (cond
                            [(init-pos . > . end)
                             (close-immediately init-pos)]
                            [else
                             (define (start-group)
                               (define line (line-start t init-pos))
                               (define init-col (+ (- init-pos line) (line-delta t line)))
                               (cond
                                 [(and (pair? (parse-state-gs state))
                                       (init-col . <= . (group-col g)))
                                  (close-immediately init-pos)]
                                 [else
                                  (define new-g (make-group init-col
                                                            #:bar-line (and (eq? use-category 'bar-operator)
                                                                            (line-orig-start t pos))))
                                  (define new-state (struct-copy parse-state state
                                                                 [gs (cons new-g
                                                                           (parse-state-gs state))]))
                                  (loop init-pos e #t new-state (cons (cons e 'open) inserts))]))
                             (define category (classify-position t init-pos))
                             (case category
                               [(closer comma-operator bar-operator)
                                (close-immediately init-pos)]
                               [(semicolon-operator)
                                (define-values (s e) (send t get-token-range init-pos))
                                (define after-pos (skip-whitespace2 t e end))
                                (cond
                                  [(and (after-pos . < . end)
                                        (eq? 'opener (classify-position t after-pos))
                                        (armor-opener? t after-pos))
                                   (start-group)]
                                  [else
                                   (close-immediately init-pos)])]
                               [else
                                (start-group)])])])])]
                   [(opener at-opener)
                    (define next-pos (skip-whitespace2 t e end))
                    (define line (line-start t next-pos))
                    (loop next-pos e #t
                          (make-parse-state (list (make-group (+ (- next-pos line)
                                                                 (line-delta t line))
                                                              #:add-semi? #f
                                                              #:close? #f
                                                              #:in-at? (eq? use-category 'at-opener)))
                                            #:skip? (or (eq? use-category 'at-opener)
                                                        (armor-opener? t s))
                                            #:prev state)
                          inserts)]
                   [else
                    (loop e e #f state inserts)]))
               (cond
                 [(and g-col
                       (col . < . g-col))
                  (loop pos prev-end #f
                        (struct-copy parse-state state
                                     [gs (cdr (parse-state-gs state))])
                        (cons (cons prev-end 'close) inserts))]
                 [(and (not init?)
                       g-col
                       (= col g-col)
                       (group-add-semi? (car (parse-state-gs state))))
                  (case category
                    [(bar-operator)
                     (cond
                       [(group-bar-line g) (close-one)]
                       [else (more-nested)])]
                    [else
                     (loop pos prev-end #t
                           state
                           ;; maybe add redundant `;` so we preserve it when unarmoring
                           (if #t ; (not (equal? ";" (send t get-text (sub1 prev-end) prev-end)))
                               (cons (cons prev-end 'semi) inserts)
                               inserts))])]
                 [(eq? category 'bar-operator)
                  (define line (line-orig-start t pos))
                  (cond
                    [(for/or ([g (in-list (parse-state-gs state))])
                       (eqv? line (group-bar-line g)))
                     (close-one)]
                    [else
                     (more-nested)])]
                 [else (more-nested)])])])))
     (send t begin-edit-sequence)
     (send t set-position (caar inserts) (caar inserts))
     (void
      (for/fold ([end end]) ([ins (in-list inserts)])
        (define pos (car ins))
        (define str (case (cdr ins)
                      [(open) "«"]
                      [(close) "»"]
                      [(enter) ";«"]
                      [(semi) ";"]
                      [else "??"]))
        (define line (line-start t pos))
        (define col (+ (- pos line) (line-delta t line)))
        (send t insert str pos)
        ;; For each later line, potentially add indentation
        ;; FIXME: this is easy, but slow
        (let loop ([line (add1 (send t position-paragraph pos))])
          (unless (line . > . end-line)
            (define s-pos (send t paragraph-start-position line))
            (when ((get-current-tab t s-pos) . >= . col)
              (unless (hash-ref skip-adjust-lines line #f)
                (send t insert (make-string (string-length str) #\space) s-pos))
              (loop (add1 line)))))))
     (send t set-position from-pos (send t get-end-position))
     (send t end-edit-sequence)]))

(define (unarmor-region t from-pos to-pos)
  (define orig-parse (parse-text t 0 from-pos to-pos))
  (cond
    [(not orig-parse)
     (void)]
    [else
     (define skip-adjust-lines (make-hasheqv))
     (define deletes
       (let loop ([pos from-pos] [prev-pos #f] [prev-category #f] [deletes '()] [stack '()])
         (cond
           [(pos . >= . to-pos) (reverse deletes)]
           [else
            (define-values (s e) (send t get-token-range pos))
            (define category (classify-position t pos))
            (case category
              [(opener)
               (cond
                 [(armor-opener? t pos)
                  (cond
                    [(and prev-pos
                          (memq prev-category '(semicolon-operator block-operator bar-operator)))
                     (loop e #f #f (cons (if (eq? prev-category 'semicolon-operator)
                                             (cons prev-pos 2)
                                             (cons pos 1))
                                         deletes)
                           (cons 'open stack))]
                    [else
                     (loop e #f #f deletes (cons 'skip stack))])]
                 [else
                  (loop e #f #f deletes stack)])]
              [(at-opener)
               (loop e #f #f deletes (cons 'at stack))]
              [(at-closer)
               (loop e #f #f deletes (if (pair? stack) (cdr stack) stack))]
              [(closer)
               (cond
                 [(armor-closer? t pos)
                  (define next-deletes
                    (if (eq? prev-category 'semicolon-operator)
                        (cons (cons prev-pos 1) deletes)
                        deletes))
                  (loop e #f #f (if (and (pair? stack)
                                         (eq? 'open (car stack)))
                                    (cons (cons pos 1) next-deletes)
                                    next-deletes)
                        (if (pair? stack) (cdr stack) stack))]
                 [else
                  (loop e #f #f deletes stack)])]
              [(whitespace)
               (cond
                 [(and (eq? prev-category 'semicolon-operator)
                       ((line-start t e) . > . (line-start t s)))
                  (loop e #f #f (cons (cons prev-pos 1) deletes) stack)]
                 [else
                  (loop e #f #f deletes stack)])]
              [(comment)
               (when (and (pair? stack) (eq? 'at (car stack)))
                 ;; line comment => don't adjust next line's indentation
                 (hash-set! skip-adjust-lines (add1 (send t position-paragraph s)) #t))
               (loop e pos category deletes stack)]
              [else
               (loop e pos category deletes stack)])])))
     (define start (line-start t from-pos))
     (define start-line (send t position-paragraph start))
     (define col (- from-pos start))
     ;; check whether this is going to work:
     (define chars (string->list
                    (string-append (make-string col #\space)
                                   (send t get-text from-pos to-pos))))
     (define (delete-leading column n line chars)
       (cond
         [(zero? n) chars]
         [(null? chars) null]
         [(char=? #\newline (car chars))
          (cond
            [(hash-ref skip-adjust-lines (add1 line) #f)
             (cons #\newline
                   (delete-leading column n (add1 line) (cdr chars)))]
            [else
             (define start-chars chars)
             (let loop ([i 0] [chars (cdr chars)])
               (cond
                 [(= i (+ column n))
                  (let loop ([i 0] [chars (delete-leading column n (add1 line) chars)])
                    (if (= i column)
                        (cons #\newline chars)
                        (loop (add1 i) (cons #\space chars))))]
                 [(and (pair? chars)
                       (char=? #\space (car chars)))
                  (loop (add1 i) (cdr chars))]
                 [else
                  ;; didn't find enough leading spaces
                  start-chars]))])]
         [else
          ;; keep looking for a newline:
          (cons (car chars) (delete-leading column n line (cdr chars)))]))
     (define new-string
       (list->string
        (let loop ([i start] [column 0] [line start-line] [chars chars] [deletes deletes])
          (cond
            [(null? deletes) chars]
            [(eqv? i (caar deletes))
             (define n (cdar deletes))
             (define rest-chars (loop (+ i n) (+ column n) line (list-tail chars n) (cdr deletes)))
             (delete-leading column n line rest-chars)]
            [else
             (define ch (car chars))
             (cons ch (loop (add1 i)
                            (if (char=? #\newline ch)
                                0
                                (add1 column))
                            (if (char=? #\newline ch)
                                (add1 line)
                                line)
                            (cdr chars)
                            deletes))]))))
     (define new-parse (parse-string new-string col))
     (cond
       [(and new-parse
             (equal? (syntax->datum orig-parse)
                     (syntax->datum new-parse)))
        (define str (substring new-string col))
        (send t begin-edit-sequence)
        (send t insert str from-pos to-pos)
        (send t set-position from-pos (+ from-pos (string-length str)))
        (send t end-edit-sequence)]
       [else
        (void)])]))

(define (parse-text t col from-pos end)
  (parse-string (string-append (make-string col #\space)
                               (send t get-text from-pos end))
                col))

(define (parse-string str col)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (define in (open-input-string (cond
                                    [(and ((string-length str) . > . 0)
                                          (char=? #\| (string-ref str col)))
                                     ;; Make a `|` form by itself parse:
                                     (string-append (make-string col #\space)
                                                    "bar"
                                                    "\n"
                                                    str)]
                                    [else str])))
    (port-count-lines! in)
    (parse-all in)))

(define (armor-opener? t pos)
  (equal? (send t get-text pos (add1 pos)) "«"))

(define (armor-closer? t pos)
  (equal? (send t get-text pos (add1 pos)) "»"))

;; todo: unify with `skip-whitespace`
(define (skip-whitespace2 t pos end
                         #:same-line? [same-line? #f]
                         #:and-own-line-group-comment [and-own-line-group-comment #f])
  (cond
    [(pos . >= . end) pos]
    [else
     (define category (classify-position t pos))
     (case category
       [(continue-operator)
        (define-values (s e) (send t get-token-range pos))
        (skip-whitespace2 t e end #:same-line? same-line? #:and-own-line-group-comment and-own-line-group-comment)]
       [(whitespace comment)
        (define-values (s e) (send t get-token-range pos))
        (cond
          [(or (not same-line?)
               (eqv? (line-start t s) (line-start t e)))
           (skip-whitespace2 t e end #:same-line? same-line? #:and-own-line-group-comment and-own-line-group-comment)]
          [else pos])]
       [(group-comment)
        (define-values (s e) (send t get-token-range pos))
        (cond
          [(and and-own-line-group-comment
                (> (line-start t s) (line-start t and-own-line-group-comment))
                (only-whitespace-between? t e (send t paragraph-end-position (send t position-paragraph e))))
           (skip-whitespace2 t e end #:same-line? same-line?)]
          [else pos])]
       [else pos])]))
