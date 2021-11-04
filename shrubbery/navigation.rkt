#lang racket/base
(require racket/class
         racket/list
         "lex.rkt"
         "private/edit-help.rkt")

;; Conventions are the same as in "indentation.rkt"

(provide shrubbery-grouping-position)

(define (shrubbery-grouping-position t pos limit-pos direction)
  (case direction
    [(backward)
     (define-values (s e) (skip-whitespace #:and-separators? #t t (sub1 pos) -1))
     (define category (classify-position t s))
     (case category
       [(closer) (send t backward-match e 0)]
       [(opener bar-operator block-operator) e]
       [else s])]
    [(forward)
     (define-values (s e) (skip-whitespace #:and-separators? #t t pos 1))
     (end-of-current t s)]
    [(down)
     (define-values (s e) (skip-whitespace #:and-separators? #t t pos 1))
     (define category (classify-position t s))
     (case category
       [(opener)
        e]
       [(block-operator bar-operator)
        e]
       [else #f])]
    [(up)
     (define pos-start (line-start t pos))
     (define-values (ns ne) (skip-whitespace t pos 1
                                             #:stay-on-line (if (only-whitespace-between? t pos-start pos)
                                                                #f
                                                                pos-start)))
     (define start (line-start t ns))
     (case (classify-position t ns)
       [(bar-operator)
        ;; immediately before a `|` is a special case
        (start-of-alts t ns)]
       [else
        (define-values (s e) (skip-whitespace t (sub1 ns) -1
                                              #:stay-on-line start))
        (define category (classify-position t s))
        (case category
          [(opener) s]
          [else
           (min pos (start-of-group #:or-out? #t t e start))])])]
    [else #f]))

;; give a `pos` that's right before a bar, return the start of the
;; first `|` in the block, or return the start of the enclsing block
;; if `pos` is at the first bar
(define (start-of-alts t pos)
  (define at-start (line-start t pos))
  (define col (col-of pos at-start (line-delta t at-start)))
  (define (outdented? s)
    (define start (line-start t s))
    (define s-col (col-of s start (line-delta t s)))
    (s-col . <= . col))
  (define (select last-bar last-block last-pos s)
    (or last-bar (and last-pos last-block) last-pos s))
  (let loop ([pos pos] ; checking before
             [last-bar #f] ; candidate bar (to move just before) most recently found (right line or column)
             [col col] ; column of most recently found `last-bar`
             [last-block #f] ; candidate `:`, only counts if `last-pos` is set for outdented
             [last-pos #f] ; candiate start of an outdented block
             [bar-start at-start] ; line for current block (to detect same-line candidates)
             [at-start at-start]) ; line for candidate block start (to detect when moved too early)
    (cond
      [(= pos 0) 0]
      [else
       (define-values (s e) (send t get-token-range (sub1 pos)))
       (define category (classify-position t s))
       (case category
         [(whitespace comment)
          (loop s last-bar col last-block last-pos
                bar-start at-start)]
         [(continue-operator)
          (define s-start (line-start t s))
          (loop s last-bar col last-block last-pos
                (if (= at-start bar-start) s-start bar-start) s-start)]
         [else
          (define s-start (line-start t pos))
          (cond
            [(and (s-start . < . at-start)
                  (select last-bar last-block last-pos #f))
             => (lambda (pos) pos)]
            [else
             (case category
               [(closer)
                (define o-s (send t backward-match e 0))
                (cond
                  [o-s
                   (loop o-s last-bar col last-block (if (outdented? o-s) o-s last-pos)
                         bar-start (line-start t o-s))]
                  [else
                   (select last-bar last-block last-pos s)])]
               [(opener)
                (select last-bar last-block last-pos s)]
               [(bar-operator)
                (define s-col (col-of s s-start (line-delta t s-start)))
                (cond
                  [(eqv? bar-start s-start)
                   ;; same line, so earlier bar the in same block
                   (loop s s s-col #f #f bar-start 0)]
                  [else
                   (cond
                     [(s-col . > . col)
                      ;; more indented: ignore
                      (loop s last-bar col last-block last-pos bar-start at-start)]
                     [(s-col . < . col)
                      ;; less indented
                      (select last-bar last-block last-pos s)]
                     [else
                      ;; same indented:
                      (loop s s col #f #f s-start 0)])])]
               [(block-operator)
                (cond
                  [(outdented? s) (select last-bar last-block last-pos s)]
                  [else (loop s last-bar col s #f bar-start s-start)])]
               [else
                (loop s last-bar col last-block (if (outdented? s) s last-pos) bar-start s-start)])])])])))

