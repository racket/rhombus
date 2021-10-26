#lang racket/base
(require "lex.rkt")

(provide lex/comment/status
         lex/comment-nested-status?)

(struct comment-tracked (status line column last-line pending depth) #:prefab)
(struct pending-comment (line column so-far) #:prefab)

(define (lex/comment-nested-status? status)
  (or (lex-nested-status? status)
      (and (comment-tracked? status)
           (lex-nested-status? (comment-tracked-status status)))))

;; wraps `lex/status` to track group comments
(define (lex/comment/status in pos status racket-lexer/status)
  (define inner-status (if (comment-tracked? status)
                           (comment-tracked-status status)
                           status))
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
  (define depth (if (comment-tracked? status)
                    (comment-tracked-depth status)
                    0))
  (define (finish pending depth
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
                       depth))
    (define new-type
      (if (and comment? (not (eq? type 'eof)))
          (hash-set (if (hash? type) type (hash 'type type)) 'comment? #t)
          type))
    (values tok new-type paren start-pos end-pos backup new-status))
  (define (finish-plain pending new-depth)
    (cond
      [(not pending) (finish pending new-depth)]
      [(memq (pending-comment-so-far pending) '(own-line in-line))
       (define (pending-new-column #:so-far [so-far 'running])
         (pending-comment line column so-far))
       (case (and (token? tok)
                  (token-name tok))
         [(bar-operator)
          ;; comment token's column doesn't matter
          (finish (pending-new-column #:so-far 'running-bar) new-depth)]
         [(comma-operator semicolor-operator closer)
          ;; comment token is bad, so stop
          (finish #f 0)]
         [else
          (cond
            [(eqv? line (pending-comment-line pending))
             ;; comment token's column determines indentation of group
             (finish (struct-copy pending-comment pending [so-far 'running]) new-depth)]
            [else
             ;; comment token, on its own line so first element determines indentation
             (finish (pending-new-column) new-depth)])])]
      [else
       (cond
         [(or (eqv? line (comment-tracked-last-line status))
              (positive? new-depth)
              (lex-nested-status? inner-status))
          ;; same line or in nested => continue comment, usually
          (case (and (token? tok)
                     (token-name tok))
            [(comma-operator semicolon-operator)
             (finish #f new-depth)]
            [(closer)
             (if (zero? depth)
                 (finish #f new-depth)
                 (finish pending new-depth))]
            [(bar-operator)
             (if (and (eq? (pending-comment-so-far pending) 'running-bar)
                      (= line (pending-comment-line pending)))
                 (finish #f 0)
                 (finish pending new-depth))]
            [else
             (finish pending new-depth)])]
         [(column . <= . (pending-comment-column pending))
          (finish #f 0)]
         [else
          (finish pending new-depth)])]))
  (define new-depth
    (case (and (token? tok) (token-name tok))
      [(opener at-opener s-exp) (add1 depth)]
      [(closer at-closer) (max 0 (sub1 depth))]
      [else depth]))
  (cond
    [(and (not pending)
          (token? tok)
          (eq? 'group-comment (token-name tok)))
     (finish (pending-comment line
                              column
                              (if (eq? line (comment-tracked-last-line status))
                                  'own-line
                                  'in-line))
             0
             #:comment? pending)]
    [(lex-nested-status? new-inner-status)
     (finish-plain pending new-depth)]
    [else
     (case (token-name tok)
       [(comment whitespace)
        (finish pending new-depth #:whitespace? #t)]
       [else
        (finish-plain pending new-depth)])]))
