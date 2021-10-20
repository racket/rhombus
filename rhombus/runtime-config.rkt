#lang racket/base

(require racket/runtime-config
         racket/port
         racket/interaction-info
         shrubbery/parse
         shrubbery/print
         shrubbery/write
         "private/set.rkt"
         (submod "private/set.rkt" for-ref))

(current-interaction-info '#((submod rhombus reader)
                             get-interaction-info
                             #f))

(current-read-interaction
 (lambda (src in)
   (when (terminal-port? in)
     (flush-output (current-output-port)))
   (define (ensure-count in)
     (if (port-counts-lines? in)
         in
         (let ([in (dup-input-port in)])
           (port-count-lines! in)
           in)))
   (parse-all (ensure-count in) #:source src #:interactive? #t)))

(print-boolean-long-form #t)

(define orig-print (global-port-print-handler))

(global-port-print-handler
 (lambda (v op [mode 0])
   (cond
     [(flonum? v)
      (cond
        [(eqv? v +inf.0) (display "#inf" op)]
        [(eqv? v -inf.0) (display "#neginf" op)]
        [(eqv? v +nan.0) (display "#nan" op)]
        [else (write v op)])]
     [(or (string? v)
          (bytes? v)
          (exact-integer? v)
          (boolean? v))
      (write v op)]
     [(struct? v)
      (define vec (struct->vector v))
      (write (object-name v) op)
      (display "(" op)
      (for ([i (in-range 1 (vector-length vec))])
        (unless (eqv? i 1) (display ", " op))
        (print (vector-ref vec i) op))
      (display ")" op)]
     [(list? v)
      (display "[" op)
      (for/fold ([first? #t]) ([e (in-list v)])
        (unless first? (display ", " op))
        (print e op)
        #f)
      (display "]" op)]
     [(pair? v)
      (display "cons(" op)
      (print (car v) op)
      (display ", " op)
      (print (cdr v) op)
      (display ")" op)]
     [(vector? v)
      (display "Array(" op)
      (for/fold ([first? #t]) ([e (in-vector v)])
        (unless first? (display ", " op))
        (print e op)
        #f)
      (display ")" op)]
     [(hash? v)
      (display "{" op)
      (for/fold ([first? #t]) ([k+v (hash-map v cons #t)])
        (define k (car k+v))
        (define v (cdr k+v))
        (unless first? (display ", " op))
        (cond
          [(keyword? k)
           (write-shrubbery k op)
           (display ": " op)
           (print v op)]
          [else
           (display "(" op)
           (print k op)
           (display ", " op)
           (print v op)
           (display ")" op)])
        #f)
      (display "}" op)]
     [(set? v)
      (display "{" op)
      (for/fold ([first? #t]) ([v (in-list (hash-map (set-ht v) (lambda (k v) k) #t))])
        (unless first? (display ", " op))
        (print v op)
        #f)
      (display "}" op)]
     [(syntax? v)
      (define s (syntax->datum v))
      (display "'" op)
      (when (and (pair? s)
                 (eq? 'op (car s)))
        (display " " op))
      (write-shrubbery s op)]
     [(procedure? v)
      (write v op)]
     [(keyword? v)
      (display "keyword(" op)
      (write-shrubbery v op)
      (display ")" op)]
     [else
      (display "#{'" op)
      (orig-print v op 1)
      (display "}" op)])))

(error-syntax->string-handler
 (lambda (s len)
   (shrubbery-syntax->string s #:max-length len)))
