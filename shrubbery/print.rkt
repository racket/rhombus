#lang racket/base
(require "private/property.rkt")

;; Printing syntax object using raw-text properties

(provide shrubbery-syntax->string)

(module+ for-parse
  (provide syntax-to-raw))

(define (shrubbery-syntax->string s
                                  #:use-raw? [use-raw? #f]
                                  #:max-length [max-length #f]
                                  #:keep-suffix? [keep-suffix? #f]
                                  #:infer-starting-indentation? [infer-starting-indentation? #t]
                                  #:register-stx-range [register-stx-range void]
                                  #:render-stx-hook [render-stx-hook (lambda (stx output) #f)])
  (cond
    [(or use-raw?
         (and (syntax? s) (all-raw-available? s)))
     (define o (open-output-string))
     (port-count-lines! o)
     (syntax-to-raw s
                    #:output o
                    #:max-length max-length
                    #:keep-suffix? keep-suffix?
                    #:register-stx-range register-stx-range
                    #:render-stx-hook render-stx-hook)
     (define orig-str (get-output-string o))
     (define starting-col (and infer-starting-indentation?
                               (extract-starting-column s)))
     ;; strip `string-col` spaces from the start of lines after the first one:
     (define str (if infer-starting-indentation?
                     (regexp-replace* (string-append "\n" (make-string starting-col #\space))
                                      orig-str
                                      "\n")
                     orig-str))
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

(define (to-output raw output max-length)
  (define (full?)
    (and max-length
         ((file-position output) . > . max-length)))
  (let loop ([l raw])
    (cond
      [(pair? l)
       (unless (full?)
         (loop (car l))
         (unless (full?)
           (loop (cdr l))))]
      [(null? l) (void)]
      [(string? l) (display l output)]
      [else (void)])))

(define (syntax-to-raw g
                       #:output [output #f]
                       #:max-length [max-length #f]
                       #:keep-suffix? [keep-suffix? #t]
                       #:register-stx-range [register-stx-range void]
                       #:render-stx-hook [render-stx-hook (lambda (stx output) #f)])
  (define (raw-cons a b) (if (and a (not (null? a)))
                             (if (and b (not (null? b)))
                                 (cons a b)
                                 a)
                             b))
  (let loop ([g g] [tail null] [use-prefix? #f] [keep-suffix? keep-suffix?])
    (cond
      [(null? g)
       (if output
           (to-output tail output max-length)
           tail)]
      [(pair? g)
       (define a-stx (car g))
       (define post (and (syntax? a-stx)
                         (syntax-raw-tail-property a-stx)))
       (define post-suffix (and (syntax? a-stx)
                                keep-suffix?
                                (syntax-raw-tail-suffix-property a-stx)))
       (define a (loop a-stx null use-prefix? (or keep-suffix?
                                                  (not (null? tail))
                                                  (not (null? (cdr g))))))
       (define d (loop (cdr g)
                       (raw-cons tail (raw-cons post post-suffix))
                       #t
                       keep-suffix?))
       (if (null? a) d (cons a d))]
      [(syntax? g)
       (define pre (and use-prefix?
                        (syntax-raw-prefix-property g)))
       (when output
         (to-output pre output max-length))
       (define (file-location-position p)
         (define-values (line col pos) (port-next-location p))
         (- pos 1))
       (define start-pos (and register-stx-range
                              output
                              (file-location-position output)))
       (define r
         (cond
           [(render-stx-hook g output)
            => (lambda (raw) raw)]
           [else
            (syntax-raw-property g)]))
       (when output
         (to-output r output max-length))
       (when start-pos
         (register-stx-range g start-pos (file-location-position output)))
       (define raw (and (not output)
                        (if (and pre r)
                            (cons pre r)
                            (or pre r null))))
       (define suffix (and (or keep-suffix?
                               (not (null? tail)))
                           (or (syntax-raw-suffix-property g)
                               null)))
       (when output
         (to-output suffix output max-length))
       (define raw+suffix
         (if (or output (null? suffix))
             raw
             (if (null? raw)
                 suffix
                 (cons raw suffix))))
       (define d (loop (syntax-e g) tail use-prefix? keep-suffix?))
       (cond
         [output (void)]
         [else
          (if (null? raw+suffix) d (cons raw+suffix d))])]
      [else
       (if output
           (to-output tail output max-length)
           tail)])))

(define (all-raw-available? s)
  (cond
    [(syntax? s)
     (or (syntax-raw-property s)
         (let ([e (syntax-e s)])
           (or (and (pair? e)
                    (all-raw-available? e))
               (null? e)))
         #;
         (and (log-error "?? ~s" s)
              #f))]
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
