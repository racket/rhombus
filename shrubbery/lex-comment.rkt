#lang racket/base
(require "lex.rkt")

(provide lex/comment/status
         lex/comment-nested-status?)

(struct comment-tracked (status line column last-line pending stack) #:prefab)
(struct pending-comment (line column so-far stack) #:prefab)

(define (lex/comment-nested-status? status)
  (or (lex-nested-status? status)
      (and (comment-tracked? status)
           (lex-nested-status? (comment-tracked-status status)))))

;; wraps `lex/status` to track group comments
(define (lex/comment/status in pos status racket-lexer/status)
  (define inner-status (if (comment-tracked? status)
                           (comment-tracked-status status)
                           status))
  (cond
    [(not (port-counts-lines? in))
     (lex/status in pos inner-status racket-lexer/status)]
    [else
     (define-values (start-line start-column start-offset) (port-next-location in))
     (define-values (tok type paren start-pos end-pos backup new-inner-status)
       (lex/status in pos inner-status racket-lexer/status))
     (define-values (end-line end-column end-offset) (port-next-location in))
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
       (member "«" (pending-comment-stack pending)))
     (define (finish pending stack
                     #:whitespace? [whitespace? #f]
                     #:comment? [comment? pending])
       (define new-status
         (comment-tracked new-inner-status
                          (+ line line-delta)
                          (if (eqv? line-delta 0)
                              (+ column (- end-column start-column))
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
     (define (finish-plain pending new-stack)
       (cond
         [(not pending) (finish pending new-stack)]
         [(memq (pending-comment-so-far pending) '(own-line in-line))
          (define (pending-new-column #:so-far [so-far 'running])
            (pending-comment line column so-far stack))
          (case (and (token? tok)
                     (token-name tok))
            [(bar-operator)
             ;; comment token's column doesn't matter
             (finish (pending-new-column #:so-far 'running-bar) new-stack)]
            [(comma-operator semicolor-operator closer)
             ;; comment token is bad, so stop
             (finish #f (pending-comment-stack pending))]
            [else
             (cond
               [(or (not-line-sensitive?)
                    (eqv? line (pending-comment-line pending)))
                ;; comment token's column determines indentation of group
                (finish (struct-copy pending-comment pending [so-far 'running]) new-stack)]
               [else
                ;; comment token, on its own line so first element determines indentation
                (finish (pending-new-column) new-stack)])])]
         [else
          (cond
            [(or (eqv? line (comment-tracked-last-line status))
                 (not-line-sensitive?)
                 (pair? new-stack)
                 (lex-nested-status? inner-status))
             ;; same line or in nested => continue comment, usually
             (case (and (token? tok)
                        (token-name tok))
               [(comma-operator semicolon-operator)
                (finish #f new-stack)]
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
            [(and (column . <= . (pending-comment-column pending))
                  (not (not-line-sensitive?)))
             (finish #f (pending-comment-stack pending))]
            [else
             (finish pending new-stack)])]))
     (define new-stack
       (case (and (token? tok) (token-name tok))
         [(opener at-opener s-exp) (cons (token-e tok) stack)]
         [(closer at-closer) (if (pair? stack) (cdr stack) '())]
         [else stack]))
     (cond
       [(and (not pending)
             (token? tok)
             (eq? 'group-comment (token-name tok)))
        (finish (pending-comment line
                                 column
                                 (if (or (not status)
                                         (and pending
                                              (not-line-sensitive?))
                                         (eq? line (comment-tracked-last-line status)))
                                     'own-line
                                     'in-line)
                                 stack)
                '()
                #:comment? pending)]
       [(lex-nested-status? new-inner-status)
        (finish-plain pending new-stack)]
       [else
        (case (token-name tok)
          [(comment whitespace)
           (finish pending new-stack #:whitespace? #t)]
          [else
           (finish-plain pending new-stack)])])]))
