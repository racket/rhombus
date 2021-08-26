#lang racket/base

(require racket/runtime-config
         racket/keyword
         racket/symbol
         shrubbery/parse)

(current-read-interaction
 (lambda (src in)
   ;; For now, read until EOF or an unindented ";" on its own line
   (define-values (i o) (make-pipe))
   (let loop ()
     (define l (read-line in))
     (unless (eof-object? l)
       (displayln l o)
       (unless (string=? l ";")
         (loop))))
   (close-output-port o)
   (port-count-lines! i)
   (define r (parse-all i #:source src))
   r))

(print-boolean-long-form #t)

(define rx:identifier #px"^(?:\\p{L}|_)(?:\\p{L}|\\p{N}|_)*$")

(define orig-print (global-port-print-handler))

(define (print-code v op #:sole? [sole? #f])
  (cond
    [(list? v)
     (cond
       [(null? v)
        (error 'print-code "unexpected ~s" v)]
       [(eq? 'op (car v))
        (cond
          [(and (not sole?)
                (memq (cadr v) '(... ¿)) )
           (display "¿(? " op)
           (display (cadr v) op)
           (display ")" op)]
          [else
           (display (cadr v) op)])]
       [(eq? 'alts (car v))
        (display "{" op)
        (for ([v (in-list (cdr v))])
          (display " | ")
          (unless (and (pair? v) (eq? (car v) 'block))
            (error 'print-code "unexpected ~s" v))
          (for/fold ([first? #t]) ([v (in-list (cdr v))])
            (unless first? (display "; " op))
            (print-code v op)
            #f))
        (display " }" op)]
       [else
        (define-values (open sep close)
          (case (car v)
            [(group) (values "" " "  "")]
            [(block) (values "{ " "; " " }")]
            [(parens) (values "(" ", " ")")]
            [(brackets) (values "[" ", " "]")]
            [else (error 'print-code "unexpected ~s" (car v))]))
        (display open op)
        (for/fold ([first? #t]) ([v (in-list (cdr v))])
          (unless first? (display sep op))
          (print-code v op)
          #f)
        (display close op)])]
    [(symbol? v)
     (define s (symbol->immutable-string v))
     (cond
       [(regexp-match? rx:identifier s)
        (display s op)]
       [else
        (display "#{")
        (write v op)
        (display "}")])]
    [else
     (print v op)]))

(global-port-print-handler
 (lambda (v op [mode 0])
   (cond
     [(or (string? v)
          (bytes? v)
          (exact-integer? v)
          (flonum? v)
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
      (display "Map(" op)
      (for/fold ([first? #t]) ([(k v) (in-hash v)])
        (unless first? (display ", " op))
        (cond
          [(and (keyword? k)
                (regexp-match? rx:identifier (keyword->immutable-string k)))
           (display "'" op)
           (display (keyword->immutable-string k) op)
           (display "': " op)
           (print v op)]
          [else
           (print k op)
           (display ", " op)
           (print v op)])
        #f)
      (display ")" op)]
     [(syntax? v)
      (define s (syntax->datum v))
      (display "?" op)
      (when (and (pair? s)
                 (eq? 'op (car s)))
        (display " " op))
      (print-code s op #:sole? #t)]
     [(procedure? v)
      (write v op)]
     [else
      (display "#{'" op)
      (orig-print v op 1)
      (display "}")])))
