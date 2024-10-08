#lang racket/base
(require "private/property.rkt"
         "write.rkt")

;; Printing syntax object using raw-text properties

(provide shrubbery-syntax->string
         shrubbery-syntax->raw
         combine-shrubbery-raw)

;; Expects `s` to be a shrubbery syntax object, but accommodates any value
;; by falling back to S-expression printing
(define (shrubbery-syntax->string s
                                  #:use-raw? [use-raw? #f]
                                  #:max-length [max-length #f]
                                  #:keep-prefix? [keep-prefix? #f]
                                  #:keep-suffix? [keep-suffix? #f]
                                  #:inner? [inner? #f]
                                  #:infer-starting-indentation? [infer-starting-indentation? #t]
                                  #:register-stx-range [register-stx-range void]
                                  #:render-stx-hook [render-stx-hook (lambda (stx output) #f)])
  (cond
    [(or use-raw?
         (and (syntax? s) (all-raw-available? s)))
     (define o (open-output-string))
     (port-count-lines! o)
     (extract/print-raw (datum->syntax #f s)
                        #:output o
                        #:max-length max-length
                        #:keep-prefix? keep-prefix?
                        #:keep-suffix? keep-suffix?
                        #:inner? inner?
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

(define (shrubbery-syntax->raw s
                               #:use-raw? [use-raw? #f]
                               #:keep-prefix? [keep-prefix? #f]
                               #:keep-suffix? [keep-suffix? #f]
                               #:inner? [inner? #f])
  (cond
    [(or use-raw?
         (and (syntax? s) (all-raw-available? s)))
     (extract/print-raw s
                        #:keep-prefix? keep-prefix?
                        #:keep-suffix? keep-suffix?
                        #:inner? inner?)]
    [else
     (values #f
             (shrubbery-syntax->string s)
             #f)]))

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

(define (combine-shrubbery-raw a b)
  (or (if (and a (not (null? a)))
          (if (and b (not (null? b)))
              (cons a b)
              a)
          b)
      null))

;; returns (values prefix content suffix)
(define (extract/print-raw g
                           #:output [output #f]
                           #:max-length [max-length #f]
                           #:keep-prefix? keep-prefix?
                           #:keep-suffix? keep-suffix?
                           #:inner? inner?
                           #:register-stx-range [register-stx-range #f]
                           #:render-stx-hook [render-stx-hook (lambda (stx output) #f)])
  (define (raw-cons a b) (combine-shrubbery-raw a b))
  (define (raw-list* a b c) (combine-shrubbery-raw (combine-shrubbery-raw a b) c))
  (let loop ([g g] [use-prefix? keep-prefix?] [keep-suffix? keep-suffix?] [inner? inner?])
    (define (get-start)
      (cond
        [(and register-stx-range
              output)
         (define start (file-location-position output))
         (values start
                 (render-stx-hook g output))]
        [else (values #f #f)]))
    (define (register start-pos)
      (when start-pos
        (register-stx-range g start-pos (file-location-position output))))

    (define (out raw)
      (cond
        [output
         (to-output raw output max-length)
         #f]
        [else
         raw]))

    (define (out/register raw)
      (cond
        [register-stx-range
         (define-values (start-pos replaced?) (get-start))
         (begin0
           (out (and (not replaced?) raw))
           (register start-pos))]
        [else (out raw)]))

    (define (s-exp g)
      (define raw (out/register
                   (list "#{" (format "~s" (syntax->datum g)) "}")))
      (values #f raw #f))

    (define (other g)
      (define o (open-output-string))
      (write-shrubbery (syntax->datum g) o)      
      (define raw (out/register (get-output-string o)))
      (values #f raw #f))

    (define (sequence l use-prefix? keep-suffix?)
      (let e-loop ([l l] [use-prefix? use-prefix?] [keep-suffix? keep-suffix?])
        (cond
          [(null? l) (values #f #f #f)]
          [(null? (cdr l))
           (loop (car l) use-prefix? keep-suffix? #t)]
          [else
           (define-values (pfx raw sfx) (loop (car l) use-prefix? #t #t))
           (define-values (rest-pfx res-raw rest-sfx) (e-loop (cdr l) #t keep-suffix?))
           (values pfx
                   (raw-cons (raw-cons raw sfx)
                             (raw-cons rest-pfx res-raw))
                   rest-sfx)])))

    (define (container a l bracketed? use-prefix? keep-suffix?)
      (define init-prefix (out (and use-prefix?
                                    (raw-cons
                                     (syntax-raw-prefix-property a)
                                     (and (not inner?)
                                          (syntax-raw-inner-prefix-property a))))))

      (define inner-prefix (out (and inner?
                                     (syntax-raw-inner-prefix-property a))))

      (define-values (start-pos replaced?) (get-start))
      (define init-raw (raw-cons
                        inner-prefix
                        (out (and (not replaced?)
                                  (syntax-raw-property a)))))

      (define end-raw (syntax-raw-tail-property a))
      (define inner-suffix (and inner?
                                (syntax-raw-inner-suffix-property a)))

      (define end-suffix (and keep-suffix?
                              (raw-cons
                               (and (not inner?)
                                    (syntax-raw-inner-suffix-property a))
                               (syntax-raw-suffix-property a))))

      (define-values (prefix raw suffix)
        (cond
          [replaced? (values init-prefix #f end-suffix)]
          [(syntax-raw-opaque-content-property a)
           => (lambda (raw)
                (values init-prefix
                        (raw-list* init-raw
                                   (out raw)
                                   (raw-cons (out end-raw)
                                             (out inner-suffix)))
                        end-suffix))]
          [else
           (define-values (pfx raw sfx) (sequence l
                                                  (or bracketed? keep-prefix?)
                                                  (or bracketed? keep-suffix?)))
           (if bracketed?
               (values init-prefix
                       (raw-cons init-raw (raw-cons (raw-cons pfx raw)
                                                    (raw-list* sfx
                                                               (out end-raw)
                                                               (out inner-suffix))))
                       end-suffix)
               (values (raw-cons init-prefix pfx)
                       (raw-list* init-raw raw (raw-cons (out end-raw) (out inner-suffix)))
                       (raw-cons sfx end-suffix)))]))

      (register start-pos)

      (when output
        ;; needs to be after `register`
        (out end-suffix))

      (values prefix raw suffix))

    (cond
      [(syntax-opaque-raw-property g)
       => (lambda (raw-in)
            (define prefix (out (and keep-prefix?
                                     (raw-cons
                                      (syntax-raw-prefix-property g)
                                      (and (not inner?)
                                           (syntax-raw-inner-prefix-property g))))))
            (define raw (raw-list*
                         (and inner?
                              (out (syntax-raw-inner-prefix-property g)))
                         (out/register raw-in)
                         (and inner?
                              (out (syntax-raw-inner-suffix-property g)))))
            (define suffix (out (and keep-suffix?
                                     (raw-cons
                                      (and (not inner?)
                                           (syntax-raw-inner-suffix-property g))
                                      (syntax-raw-suffix-property g)))))
            (values prefix raw suffix))]
      [(pair? (syntax-e g))
       (define l (syntax->list g))
       (cond
         [(not l) (s-exp g)]
         [else
          (define a (car l))
          (case (syntax-e a)
            [(group multi)
             (container a (cdr l) #f use-prefix? keep-suffix?)]
            [(op)
             (if (and (pair? (cdr l)) (null? (cddr l)))
                 (loop (cadr l) use-prefix? keep-suffix? inner?)
                 (s-exp g))]
            [(parens brackets braces quotes block alts)
             (container a (cdr l) #t use-prefix? keep-suffix?)]
            [(parsed)
             (cond
               [(and (= 3 (length l))
                     (syntax-opaque-raw-property (caddr l)))
                (loop (caddr l) use-prefix? keep-suffix? inner?)]
               [else
                (s-exp g)])]
            [else (s-exp g)])])]
      [(syntax-raw-property g)
       => (lambda (raw)
            (container g null #f use-prefix? keep-suffix?))]
      [else
       (other g)])))

(define (all-raw-available? s)
  (let loop ([s s])
    (or
     (cond
       ;; allow anything that has been specifically designated as raw and opaque:
       [(syntax-opaque-raw-property s)
        #t]
       ;; otherwise, traverse the shrubbery encoding
       [(pair? (syntax-e s))
        (define l (syntax->list s))
        (define (rest-available? l)
          (for/and ([e (in-list l)])
            (all-raw-available? e)))
        (cond
          [(not l) #f]
          [else
           (case (syntax-e (car l))
             [(multi)
              (rest-available? (cdr l))]
             [(group)
              (or (syntax-opaque-raw-property (car l))
                  (rest-available? (cdr l)))]
             [(op)
              (and (pair? (cdr l))
                   (null? (cddr l))
                   (loop (cadr l)))]
             [(parens brackets braces quotes block alts)
              (or (syntax-opaque-raw-property (car l))
                  (and (syntax-raw-property (car l))
                       (rest-available? (cdr l))))]
             [(parsed)
              (= 3 (length l))]
             [else #f])])]
       [else (syntax-raw-property s)])
     #;
     (and (log-error "?? ~s" s)
          #f))))

(define (extract-starting-column s)
  (cond
    [(syntax? s)
     (or (syntax-column s)
         (let ([e (syntax-e s)])
           (and (pair? e)
                (extract-starting-column (car e))))
         0)]
    [else 0]))

(define (file-location-position p)
  (define-values (line col pos) (port-next-location p))
  (- pos 1))
