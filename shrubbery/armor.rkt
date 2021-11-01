#lang racket/base
(require racket/class
         "private/edit-help.rkt")

(provide toggle-armor
         armor-region)

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
  (armor-region t
                (send t get-start-position)
                (send t get-end-position)))
  
(define (armor-region t from-pos to-pos)
  (define start (line-start t from-pos))
  (define end-start-line (send t position-paragraph to-pos))
  (define end (send t paragraph-end-position end-start-line))
  (define end-line (if (= (send t paragraph-start-position end-start-line) end)
                       (sub1 end-start-line)
                       end-start-line))
  (define skip-adjust-lines (make-hasheqv))
  (define col (get-current-tab t start))
  (define init (+ start col))
  (define gs (list (make-group col)))
  (define inserts
    (let loop ([pos init] [prev-end init] [init? #t] [state (make-parse-state gs)] [inserts (list (cons init 'enter))])
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
              ;; line comment => don't adjust next lines indentation
              (hash-set! skip-adjust-lines (add1 (send t position-paragraph s)) #t))
            (loop e prev-end #f state inserts)]
           [(group-comment)
            (define next-pos (skip-whitespace t e end))
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
                 (define next-pos (skip-whitespace t e end #:same-line? #t))
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
                       (define init-pos (skip-whitespace t next-pos end #:and-own-line-group-comment next-pos))
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
                             (define after-pos (skip-whitespace t e end))
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
                 (define next-pos (skip-whitespace t e end))
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
  (send t end-edit-sequence))

(define (armor-opener? t pos)
  (equal? (send t get-text pos (add1 pos)) "«"))

(define (skip-whitespace t pos end
                         #:same-line? [same-line? #f]
                         #:and-own-line-group-comment [and-own-line-group-comment #f])
  (cond
    [(pos . >= . end) pos]
    [else
     (define category (classify-position t pos))
     (case category
       [(continue-operator)
        (define-values (s e) (send t get-token-range pos))
        (skip-whitespace t e end #:same-line? same-line? #:and-own-line-group-comment and-own-line-group-comment)]
       [(whitespace comment)
        (define-values (s e) (send t get-token-range pos))
        (cond
          [(or (not same-line?)
               (eqv? (line-start t s) (line-start t e)))
           (skip-whitespace t e end #:same-line? same-line? #:and-own-line-group-comment and-own-line-group-comment)]
          [else pos])]
       [(group-comment)
        (define-values (s e) (send t get-token-range pos))
        (cond
          [(and and-own-line-group-comment
                (> (line-start t s) (line-start t and-own-line-group-comment))
                (only-whitespace-between? t e (send t paragraph-end-position (send t position-paragraph e))))
           (skip-whitespace t e end #:same-line? same-line?)]
          [else pos])]
       [else pos])]))
