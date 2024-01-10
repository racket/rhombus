#lang racket/base
(require pict)

(provide para)

;; Extracted from Slideshow with small adjustments. Probably would be
;; better to rewrite it in Rhombus

(define (para #:t t
              #:width width
              #:line-sep [line-sep 0]
              #:align [align 'left]
              #:fill? [fill? #f]
              #:decode? [decode? #t]
              . s)
  (let ([p (para* (case align
                    [(right) vr-append]
                    [(center) vc-append]
                    [else vl-append])
                  width
                  line-sep
                  t
                  (if decode?
                      (decode s)
                      s))])
    (if fill?
        ((case align
           [(right) rtl-superimpose]
           [(center) ctl-superimpose]
           [else ltl-superimpose])
         (blank width 0)
         p)
        p)))

(define (para* v-append w line-sep t . s)
  (define space (t " "))
  (let loop ([pre #f][s (shift-no-sep t s)][rest null])
    (cond
      [(null? s)
       (if (null? rest)
           (or pre (blank))
           (loop pre (car rest) (cdr rest)))]
      [(list? s) (loop pre (car s) (append (cdr s) rest))]
      [else
       (let* ([p (if (string? s) (t s) s)])
         (cond
           [(< (+ (if pre (pict-width pre) 0)
                  (if pre (pict-width space) 0)
                  (pict-width p)) 
               w)
            ;; small enough
            (loop (if pre 
                      (hbl-append pre space p) 
                      p)
                  rest null)]
           [(and (string? s) (regexp-match "(.*) (.*)" s))
            ;; can break on string
            => (lambda (m)
                 (loop pre
                       (cadr m) 
                       (cons
                        (caddr m)
                        rest)))]
           [(not pre)
            (if (null? rest)
                p
                (v-append
                 line-sep
                 p
                 (loop #f rest null)))]
           [else
            (v-append
             line-sep
             pre
             (loop p rest null))]))])))

(define (decode s)
  (let loop ([s s])
    (cond
      [(list? s) 
       (map 
        decode
        ;; Remove "\n", and also cancel extra spaces after "\n":
        (let loop ([s s])
          (cond
            [(null? s) null]
            [(equal? (car s) "\n")
             (let nloop ([s (cdr s)])
               (if (and (pair? s)
                        (string? (car s)))
                   (let ([a (regexp-replace #rx"^ +" (car s) "")])
                     (if (string=? a "")
                         (nloop (cdr s))
                         (loop (cons a (cdr s)))))
                   (loop s)))]
            [else (cons (car s) (loop (cdr s)))])))]
      [(not (string? s)) s]
      [(regexp-match-positions #rx"---" s)
       => (lambda (m)
            (string-append (loop (substring s 0 (caar m)))
                           "\u2014"
                           (loop (substring s (cdar m)))))]
      [(regexp-match-positions #rx"--" s)
       => (lambda (m)
            (string-append (loop (substring s 0 (caar m)))
                           "\u2013"
                           (loop (substring s (cdar m)))))]
      [(regexp-match-positions #px"``" s)
       => (lambda (m)
            (string-append (loop (substring s 0 (caar m)))
                           "\u201C"
                           (loop (substring s (cdar m)))))]
      [(regexp-match-positions #px"''" s)
       => (lambda (m)
            (string-append (loop (substring s 0 (caar m)))
                           "\u201D"
                           (loop (substring s (cdar m)))))]
      [(regexp-match-positions #rx"'" s)
       => (lambda (m)
            (string-append (loop (substring s 0 (caar m)))
                           "\u2019"
                           (loop (substring s (cdar m)))))]           
      [else s])))

;; Move separators (that shouldn't be preceded by extra space)
;; at the front of a string to the end of the previous item
(define (shift-no-sep t l)
  (let loop ([l 
              ;; Flatten l, first:
              (let loop ([l l])
                (cond
                  [(null? l) null]
                  [(pair? (car l)) (append (loop (car l)) (loop (cdr l)))]
                  [else (cons (car l) (loop (cdr l)))]))]
             [a null])
    ;; Combine strings:
    (cond
      [(null? l) (reverse a)]
      [(null? a) (loop (cdr l) (list (car l)))]
      [(and (string? (car l)) 
            (regexp-match #rx"^[-',. :;?!)\U201D\U2019]" (car l)))
       (let ([m (regexp-match #rx"^([^ ]*) (.*)$" (car l))])
         (if m
             (if (string? (car a))
                 (loop (cdr l)
                       (list* (caddr m)
                              (string-append (car a) (cadr m))
                              (cdr a)))
                 (loop (cdr l)
                       (list* (caddr m)
                              (hbl-append (car a) (t (cadr m)))
                              (cdr a))))
             (if (string? (car a))
                 (loop (cdr l)
                       (cons (string-append (car a) (car l))
                             (cdr a)))
                 (loop (cdr l)
                       (cons (hbl-append (car a) (t (car l)))
                             (cdr a))))))]
      [else (loop (cdr l) (cons (car l) a))])))
