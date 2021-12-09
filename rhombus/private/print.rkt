#lang racket/base
(require racket/symbol
         racket/keyword
         shrubbery/write
         (submod "set.rkt" for-ref))

(provide (rename-out
          [rhombus-print print]
          [rhombus-display display])
         println
         displayln
         (rename-out
          [current-output-port current_output_port]))

(module+ redirect
  (provide (struct-out racket-print-redirect)))

(define (rhombus-print v [op (current-output-port)])
  (do-print v op 'print))

(define (rhombus-display v [op (current-output-port)])
  (do-print v op 'display))

(define (println v [op (current-output-port)])
  (rhombus-print v op)
  (newline op))

(define (displayln v [op (current-output-port)])
  (rhombus-display v op)
  (newline op))

(define (do-print v op mode)
  (let loop ([v v] [mode mode])
    (define (display?) (eq? mode 'display))
    (define (print v) (loop v 'print))
    (cond
      [(flonum? v)
       (cond
         [(eqv? v +inf.0) (display "#inf" op)]
         [(eqv? v -inf.0) (display "#neginf" op)]
         [(eqv? v +nan.0) (display "#nan" op)]
         [else (write v op)])]
      [(or (string? v)
           (bytes? v)
           (exact-integer? v))
       (write v op)]
      [(boolean? v)
       (display (if v "#true" "#false") op)]
      [(void? v)
       (display "#void" op)]
      [(struct? v)
       (define vec (struct->vector v))
       (write (object-name v) op)
       (display "(" op)
       (for ([i (in-range 1 (vector-length vec))])
         (unless (eqv? i 1) (display ", " op))
         (print (vector-ref vec i)))
       (display ")" op)]
      [(list? v)
       (display "[" op)
       (for/fold ([first? #t]) ([e (in-list v)])
         (unless first? (display ", " op))
         (print e)
         #f)
       (display "]" op)]
      [(pair? v)
       (display "cons(" op)
       (print (car v))
       (display ", " op)
       (print (cdr v))
       (display ")" op)]
      [(vector? v)
       (display "Array(" op)
       (for/fold ([first? #t]) ([e (in-vector v)])
         (unless first? (display ", " op))
         (print e)
         #f)
       (display ")" op)]
      [(hash? v)
       (display "{" op)
       (for/fold ([first? #t]) ([k+v (hash-map v cons #t)])
         (define k (car k+v))
         (define v (cdr k+v))
         (unless first? (display ", " op))
         (print k)
         (display ": " op)
         (print v)
         #f)
       (display "}" op)]
      [(set? v)
       (display "{" op)
       (for/fold ([first? #t]) ([v (in-list (hash-map (set-ht v) (lambda (k v) k) #t))])
         (unless first? (display ", " op))
         (print v)
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
      [(symbol? v)
       (cond
         [(display?)
          (display (symbol->immutable-string v))]
         [else
          (display "symbol(" op)
          (write-shrubbery v op)
          (display ")" op)])]
      [(keyword? v)
       (cond
         [(display?)
          (display (keyword->immutable-string v))]
         [else
          (display "keyword(" op)
          (write-shrubbery v op)
          (display ")" op)])]
      [else
       (cond
         [(display?)
          (display v op)]
         [else
          (display "#{'" op)
          (racket-print (racket-print-redirect v) op 1)
          (display "}" op)])])))

(define (racket-print v op mode)
  (print v op mode))

(struct racket-print-redirect (val)
  #:property prop:custom-write
  (lambda (r op mode)
    (racket-print (racket-print-redirect-val r) op mode)))
