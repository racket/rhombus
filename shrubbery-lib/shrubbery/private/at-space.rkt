#lang racket/base
(require "property.rkt")

(provide adjust-content-space)

(define (adjust-content-space rev group-tag)
  (let*-values ([(rev) (remove-trailing-spaces rev)]
                [(rev end2) (discard-immediate-space rev)]
                [(rev end1) (discard-immediate-newline rev #f)])
    (let*-values ([(content) (reverse rev)]
                  [(content start1) (discard-immediate-space content)]
                  [(content start2) (discard-immediate-newline content #f)])
      (let* ([min-col (get-min-column content)]
             [content (trim-shared-column content min-col (pair? start2))])
        (let-values ([(content comments) (convert-content content group-tag)])
          (values (append start1 start2)
                  content
                  (append (map to-syntax (reverse comments)) end1 end2)))))))

(define (to-syntax str)
  (syntax-raw-property (datum->syntax #f str) str))

(define rx:whitespace #px"^\\s*$")

(define (is-newline? c)
  (and (eq? 'content (car c))
       (equal? "\n" (syntax-e (cadr c)))))

;; discard space immediately before closer or after opener
(define (discard-immediate-space lst)
  (cond
    [(and (pair? lst)
          (eq? 'content (caar lst))
          (pair? (cdr lst))
          (not (equal? (syntax-e (cadar lst)) "\n"))
          (let ([prev (cadr lst)])
            (and (eq? 'content (car prev))
                 (equal? "\n" (syntax-e (cadr prev)))))
          (regexp-match? rx:whitespace (syntax-e (cadar lst))))
     (values (cdr lst) (cdar lst))]
    [else
     (values lst null)]))

;; discard newline just before closer,
;; unless a newline is all there will be
(define (discard-immediate-newline lst check-next-ws?)
  (cond
    [(and (pair? lst)
          (is-newline? (car lst))
          (not (null? (cdr lst)))
          (not (and check-next-ws?
                    (null? (cddr lst))
                    (let ([prev (cadr lst)])
                      (and (eq? 'content (car prev))
                           (regexp-match? rx:whitespace (cadr prev)))))))
     (values (cdr lst) (cdar lst))]
    [else
     (values lst null)]))

(define (remove-trailing-spaces rev)
  (define (trailing-space-count s)
    (let loop ([i (string-length s)])
      (cond
        [(eqv? i 0) 0]
        [(char-whitespace? (string-ref s (sub1 i))) (add1 (loop (sub1 i)))]
        [else 0])))
  (let loop ([rev rev])
    (cond
      [(null? rev) rev]
      [(is-newline? (car rev))
       (define next (cdr rev))
       (cons (car rev)
             (cond
               [(null? next) null]
               [(is-newline? (car next)) (loop next)]
               [(eq? 'content (caar next))
                (define n (trailing-space-count (syntax-e (cadar next))))
                (cond
                  [(eqv? n 0) (loop next)]
                  [else
                   (define a (cadar next))
                   (define s (syntax-e a))
                   (define len (- (string-length s) n))
                   (cons (list 'content
                               (datum->syntax a
                                              (substring s 0 len)
                                              a
                                              a))
                         (loop (cdr next)))])]
               [else (loop next)]))]
      [else (cons (car rev) (loop (cdr rev)))])))

(define (leading-space-count s)
  (let loop ([i 0])
    (cond
      [(eqv? i (string-length s)) 0]
      [(char-whitespace? (string-ref s i)) (add1 (loop (add1 i)))]
      [else 0])))

(define (get-min-column lst)
  (let loop ([lst lst] [min-col #f] [saw-nl? #t])
    (cond
      [(null? lst) min-col]
      [(is-newline? (car lst))
       (loop (cdr lst) min-col #t)]
      [(and saw-nl?
            (eq? 'content (caar lst)))
       (define stx (cadar lst))
       (define s (syntax-e stx))
       (cond
         [(equal? s "")
          ;; removing trailing whitespace made the line blank
          (loop (cdr lst) min-col saw-nl?)]
         [else
          (define n (+ (leading-space-count s) (or (syntax-column stx) 0)))
          (loop (cdr lst) (min n (or min-col n)) #f)])]
      [else
       (loop (cdr lst) (or min-col 0) #f)])))

;; also splits leading whitespace into its own element
(define (trim-shared-column lst min-col start-nl?)
  (let loop ([lst lst] [saw-nl? start-nl?])
    (cond
      [(null? lst) null]
      [(is-newline? (car lst))
       (cons (car lst)
             (loop (cdr lst) #t))]
      [(and saw-nl?
            (eq? 'content (caar lst)))
       (define a (cadar lst))
       (define s (syntax-e a))
       (define col (or (syntax-column a) 0))
       (define n (leading-space-count s))
       (define add-back-n (max 0 (- n min-col)))
       (define trimmed-s (if (eqv? n 0)
                             s
                             (substring s n)))
       (define rest-trimmed (loop (cdr lst) #f))
       (define drop-trimmed-s? (eqv? 0 (string-length trimmed-s)))
       (define trimmed
         (if drop-trimmed-s?
             rest-trimmed
             (cons (list 'content
                         (datum->syntax a
                                        trimmed-s
                                        a
                                        a))
                   rest-trimmed)))
       (if (eqv? add-back-n 0)
           (if drop-trimmed-s?
               (add-as-prefix a trimmed)
               trimmed)
           (cons (list 'content
                       (let ([stx (datum->syntax a
                                                 (make-string add-back-n #\space)
                                                 a
                                                 (and drop-trimmed-s?
                                                      a))])
                         (if drop-trimmed-s?
                             stx
                             (syntax-raw-property stx '()))))
                 trimmed))]
      [(eq? 'comment (caar lst))
       (cons (car lst) (loop (cdr lst) saw-nl?))]
      [else (cons (car lst) (loop (cdr lst) #f))])))

(define (convert-content lst group-tag)
  (let loop ([lst lst] [accum '()] [comments '()])
  (cond
    [(null? lst) (values (reverse accum) (reverse comments))]
    [else
     (define c (car lst))
     (cond
       [(eq? 'comment (car c))
        (loop (cdr lst) accum (cons (cadr c) comments))]
       [(eq? 'content (car c))
        (loop (cdr lst)
              (cons (add-comments (cons group-tag (cdr c))
                                  comments)
                    accum)
              null)]
       [else
        (loop (cdr lst)
              (cons (add-comments c comments)
                    accum)
              null)])])))

(define (add-comments c comments)
  (cond
    [(null? comments) c]
    [else
     (define stx (car c))
     (cons (syntax-raw-prefix-property stx
                                       (cons (reverse comments)
                                             (or (syntax-raw-prefix-property stx) '())))
           (cdr c))]))


(define (add-as-prefix n trimmed)
  (define prefix (syntax-raw-property n))
  (cond
    [(and (pair? trimmed)
          (eq? (caar trimmed) 'comment))
     (cons (list 'comment
                 (string-append prefix
                                (cadar trimmed)))
           (cdr trimmed))]
    [else
     (cons (list 'comment prefix) trimmed)]))
