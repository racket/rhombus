#lang racket/base
(require "lex.rkt"
         "private/column.rkt")

(provide lex/comment/status
         lex/comment-nested-status?
         lex/comment-dont-stop-status?)

(struct comment-tracked (status line column last-line pending stack) #:prefab)
(struct pending-comment (line column so-far stack bar?) #:prefab)

;; wraps `lex/status` to track group comments
(define (lex/comment/status in pos status racket-lexer/status)
  (define inner-status (if (comment-tracked? status)
                           (comment-tracked-status status)
                           status))
  (cond
    [(not (port-counts-lines? in))
     (lex/status in pos inner-status racket-lexer/status)]
    [else
     (define-values (tok type paren start-pos end-pos backup new-inner-status)
       (lex/status in pos inner-status racket-lexer/status))
     (define-values (start-line start-column end-line end-column)
       (cond
         [(token? tok) (values (token-line tok) (column-floor (token-column tok))
                               (token-end-line tok) (column-floor (token-end-column tok)))]
         [else (values 0 0 0 0)]))
     (define line-delta (- end-line start-line))
     (define line (if (comment-tracked? status)
                      (comment-tracked-line status)
                      0))
     (define column (if (comment-tracked? status)
                        (comment-tracked-column status)
                        0))
     (define pending (and (comment-tracked? status)
                          (comment-tracked-pending status)))
     (define stack (if (comment-tracked? status)
                       (comment-tracked-stack status)
                       '()))
     (define (not-line-sensitive?)
       (let loop ([stack (pending-comment-stack pending)])
         (define l (member "«" stack))
         (and l (or (null? (cdr l))
                    (not (equal? "'" (cadr l)))
                    (loop (cddr l))))))
     (define (finish pending stack
                     #:whitespace? [whitespace? #f]
                     #:comment? [comment? pending])
       (define new-status
         (comment-tracked new-inner-status
                          (+ line line-delta)
                          (if (eqv? line-delta 0)
                              (column+ column (column- end-column start-column))
                              end-column)
                          (if whitespace?
                              (if (comment-tracked? status)
                                  (comment-tracked-last-line status)
                                  0)
                              line)
                          pending
                          stack))
       (define new-type
         (if (and comment? (not (eq? type 'eof)))
             (hash-set (if (hash? type) type (hash 'type type)) 'comment? #t)
             type))
       (values tok new-type paren start-pos end-pos backup new-status))
     (define (finish-plain pending new-stack new-stack-for-end)
       (cond
         [(not pending) (finish pending new-stack)]
         [(memq (pending-comment-so-far pending) '(own-line in-line))
          (define name (and (token? tok)
                            (token-name tok)))
          (define (pending-new-column #:so-far [so-far 'running])
            (pending-comment line column so-far stack (eq? name 'bar-operator)))
          (define (continue-running)
            (finish (struct-copy pending-comment pending [so-far 'running]) new-stack))
          (case name
            [(comment)
             (finish pending new-stack #:whitespace? #t)]
            [(bar-operator)
             (if (not-line-sensitive?)
                 (continue-running)
                 ;; comment token's column doesn't matter
                 (finish (pending-new-column #:so-far 'running-bar) new-stack))]
            [(comma-operator semicolon-operator closer)
             ;; comment token is bad, so stop
             (finish #f (pending-comment-stack pending))]
            [else
             (cond
               [(or (not-line-sensitive?)
                    (eqv? line (pending-comment-line pending)))
                ;; comment token's column determines indentation of group
                (continue-running)]
               [else
                ;; comment token, on its own line so first element determines indentation
                (finish (pending-new-column) new-stack)])])]
         [else
          (cond
            [(or (eqv? line (comment-tracked-last-line status))
                 (not-line-sensitive?)
                 (pair? new-stack-for-end)
                 (lex-nested-status? inner-status))
             ;; same line or in nested => continue comment, usually
             (case (and (token? tok)
                        (token-name tok))
               [(comment)
                (finish pending new-stack #:whitespace? #t)]
               [(comma-operator semicolon-operator)
                (finish (if (null? stack) #f pending) new-stack)]
               [(closer)
                (if (null? stack)
                    (finish #f new-stack)
                    (finish pending new-stack))]
               [(bar-operator)
                (if (and (eq? (pending-comment-so-far pending) 'running-bar)
                         (or (not (not-line-sensitive?))
                             (= line (pending-comment-line pending))))
                    (finish #f (pending-comment-stack pending))
                    (finish pending new-stack))]
               [else
                (finish pending new-stack)])]
            [(and (column<=? (column+ column (if (and (not (pending-comment-bar? pending))
                                                      (token? tok)
                                                      (eq? (token-name tok) 'bar-operator))
                                                 0.5
                                                 0))
                             (pending-comment-column pending))
                  (not (not-line-sensitive?)))
             (case (and (token? tok)
                        (token-name tok))
               [(comment)
                (finish pending new-stack #:whitespace? #t #:comment? #f)]
               [else
                (finish #f (pending-comment-stack pending))])]
            [else
             (case (and (token? tok)
                        (token-name tok))
               [(comment)
                (finish pending new-stack #:whitespace? #t)]
               [else
                (finish pending new-stack)])])]))
     (define (ends-only-pending? pending new-stack-for-end)
       ;; Duplicates some of `finish-plain`, peeking ahead for what the function
       ;; will do with `pending` in the case that `tok` is a 'group-comment
       (cond
         [(memq (pending-comment-so-far pending) '(own-line in-line)) #f]
         [else
          (cond
            [(or (eqv? line (comment-tracked-last-line status))
                 (not-line-sensitive?)
                 (pair? new-stack-for-end)
                 (lex-nested-status? inner-status))
             #f]
            [(and (column . column<=? . (pending-comment-column pending))
                  (not (not-line-sensitive?)))
             #t]
            [else #f])]))
     (define-values (new-stack-for-end new-stack)
       ;; `new-stack-for-end` is for detecting whether the current comment should end
       (case (and (token? tok) (token-name tok))
         [(opener at-opener s-exp)
          (values stack (cons (token-e tok) stack))]
         [(closer at-closer)
          (define new-stack (if (pair? stack) (cdr stack) '()))
          (values new-stack new-stack)]
         [else (values stack stack)]))
     (cond
       [(and (token? tok)
             (eq? 'group-comment (token-name tok))
             (or (not pending)
                 ;; does it end the current pending comment, while
                 ;; also starting this new one?
                 (ends-only-pending? pending new-stack-for-end)))
        (finish (pending-comment line
                                 column
                                 (if (or (not status)
                                         (eq? line (comment-tracked-last-line status)))
                                     'own-line
                                     'in-line)
                                 stack
                                 #f)
                '()
                #:comment? pending)]
       [(lex-nested-status? new-inner-status)
        (finish-plain pending new-stack new-stack-for-end)]
       [else
        ;; Whitespace and comments don't affect the commenting state,
        ;; but we need to heuristcially choose whether to count as comment
        ;; as itself faded or not
        (case (token-name tok)
          [(whitespace)
           (finish pending new-stack #:whitespace? #t)]
          [else
           (finish-plain pending new-stack new-stack-for-end)])])]))

(define (lex/comment-nested-status? status)
  (or (lex-nested-status? status)
      (and (comment-tracked? status)
           (lex-nested-status? (comment-tracked-status status)))))

(define (lex/comment-dont-stop-status? status)
  (or (lex-dont-stop-status? status)
      (and (comment-tracked? status)
           (lex-dont-stop-status? (comment-tracked-status status)))))
