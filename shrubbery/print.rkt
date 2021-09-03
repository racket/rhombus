#lang racket/base

(provide shrubbery-syntax->string)

(module+ for-parse
  (provide syntax-to-raw))

(define (shrubbery-syntax->string s #:max-length [max-length #f])
  (cond
    [(all-raw-available? s)
     (define o (open-output-string))
     (define (full?)
       (and max-length
            ((file-position o) . > . max-length)))
     (let loop ([l (syntax-to-raw s)])
       (cond
         [(pair? l)
          (unless (full?)
            (loop (car l))
            (unless (full?)
              (loop (cdr l))))]
         [(null? l) (void)]
         [(string? l) (display l o)]
         [else (void)]))
     (define orig-str (get-output-string o))
     (define starting-col (extract-starting-column s))
     ;; strip `string-col` spaces from the start of lines after the first one:
     (define str (regexp-replace* (string-append "\n" (make-string starting-col #\space))
                                  orig-str
                                  "\n"))
     (if (and max-length
              ((string-length str) . > . max-length))
         (string-append (substring str 0 (max 0 (- max-length 3)))
                        "...")
         str)]
    [else
     (define v (if (syntax? s) (syntax->datum s) s))
     (if max-length
         (parameterize ([error-print-width max-length])
           (format "~.s" v))
         (format "~s" v))]))

(define (syntax-to-raw g)
  (let loop ([g g] [tail null] [use-prefix? #f])
    (cond
      [(null? g) tail]
      [(pair? g)
       (define a-stx (car g))
       (define post (syntax-property a-stx 'raw-tail))
       (define a (loop a-stx null use-prefix?))
       (define d (loop (cdr g)
                       (if post
                           (if (null? tail)
                               post
                               (cons tail post))
                           tail)
                       #t))
       (if (null? a) d (cons a d))]
      [(syntax? g)
       (define pre (and use-prefix?
                        (syntax-property g 'raw-prefix)))
       (define r (syntax-property g 'raw))
       (define raw (if (and pre r)
                       (cons pre r)
                       (or pre r null)))
       (define d (loop (syntax-e g) tail use-prefix?))
       (if (null? raw) d (cons raw d))]
      [else tail])))

(define (all-raw-available? s)
  (cond
    [(syntax? s)
     (or (syntax-property s 'raw)
         (let ([e (syntax-e s)])
           (or (and (pair? e)
                    (all-raw-available? e))
               (null? e))))]
    [(pair? s) (and (all-raw-available? (car s))
                    (all-raw-available? (cdr s)))]
    [else #t]))

(define (extract-starting-column s)
  (cond
    [(syntax? s)
     (or (syntax-column s)
         (let ([e (syntax-e s)])
           (and (pair? e)
                (extract-starting-column (car e))))
         0)]
    [else 0]))
