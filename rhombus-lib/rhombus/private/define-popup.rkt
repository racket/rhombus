#lang racket/base
(require racket/class)

;; support for `drracket:define-popup` as registered in "core.rkt"

(provide define-popup)

(define (find-word text tag-string pos default-find-word)
  (default-find-word text tag-string pos #:case-sensitive? #t #:delimited? #t))

(define (find-word/id-after text tag-string pos default-find-word)
  (let loop ([pos pos])
    (define next-pos (find-word text tag-string pos default-find-word))
    (and next-pos
         (let ([end-pos (+ next-pos (string-length tag-string))])
           (if (find-function-id-start text end-pos)
               next-pos
               (loop end-pos))))))

(define (find-function-id-start text pos)
  (let loop ([pos pos])
    (case (send text classify-position pos)
      [(comment white-space bar-operator)
       (define-values (start end) (send text get-token-range pos))
       (loop end)]
      [(symbol) pos]
      [else #f])))

(define (get-binding-string text pos default)
  ;; get everything up to a `=` or `:`
  (define start-pos
    (let loop ([pos pos])
      (case (send text classify-position pos)
        [(comment white-space)
         (define-values (start end) (send text get-token-range pos))
         (loop end)]
        [else pos])))
  (define end-pos
    (let loop ([pos start-pos] [end-pos start-pos] [openers '()])
      (define-values (start end) (send text get-token-range pos))
      (case (send text classify-position pos)
        [(block-operator) (if (null? openers)
                              pos
                              (loop end end-pos openers))]
        [(comment white-space) (loop end end-pos openers)]
        [(parenthesis)
         (define paren (send text get-text start end))
         (loop end end (cond
                         [(member paren '("(" "[" "{"))
                          (cons paren openers)]
                         [(member paren '(")" "]" "}"))
                          ;; assuming an match to openers
                          (if (pair? openers) (cdr openers) null)]
                         [(member paren '("'"))
                          (if (and (pair? openers)
                                   (equal? (car openers) paren))
                              (cdr openers)
                              (cons paren openers))]
                         [else
                          ;; other parentheses??
                          openers]))]
        [(operator) (if (and (null? openers)
                             (equal? "=" (send text get-text start end)))
                        pos
                        (loop end end openers))]
        [else (loop end end openers)])))
  (send text get-text start-pos end-pos))

(define (get-function-name-string text pos default)
  (define start-pos (find-function-id-start text pos))
  (cond
    [start-pos
     (define-values (start end) (send text get-token-range start-pos))
     (send text get-text start end)]
    [else "???"]))

;; ----------------------------------------

(define define-popup
  `(("def" "def" "δ" ,find-word ,get-binding-string)
    ("fun" "fun" "δ" ,find-word/id-after ,get-function-name-string)
    ("class" "class" "δ" ,find-word #f)))
