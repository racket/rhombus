#lang racket/base
(require racket/symbol
         racket/keyword
         shrubbery/write
         "provide.rkt"
         (submod "set.rkt" for-ref)
         "adjust-name.rkt"
         "printer-property.rkt"
         "define-arity.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         "expression.rkt"
         "mutability.rkt")

(provide (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [rhombus-print print]
                      [rhombus-display display])
                     println
                     displayln
                     print_to_string
                     (rename-out
                      [current-output-port current_output_port]
                      [current-error-port current_error_port])))

(module+ redirect
  (provide (struct-out racket-print-redirect)))

(module+ for-class
  (provide prop:print-field-shapes))

(module+ for-string
  (provide (rename-out [do-display display])))

(module+ for-runtime
  (provide (rename-out [do-print print])))

(define-values (prop:print-field-shapes print-field-shapes? print-field-shapes-ref)
  (make-struct-type-property 'print-field-shapes))

(define/arity #:name print (rhombus-print v [op (current-output-port)])
  (do-print v op 'print))

(define/arity #:name display (rhombus-display v [op (current-output-port)])
  (do-print v op 'display))

(define/arity (println v [op (current-output-port)])
  (do-print v op 'print)
  (newline op))

(define/arity (displayln v [op (current-output-port)])
  (do-print v op 'display)
  (newline op))

(define/arity (print_to_string v)
  (define op (open-output-string))
  (do-print v op 'print)
  (get-output-string op))

(define (do-display v op)
  (do-print v op 'display))

(define (do-print v op [mode 'print])
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
      [(or (and (string? v) (immutable? v))
           (and (bytes? v) (immutable? v))
           (exact-integer? v))
       (cond
         [(display?) (display v op)]
         [else (write v op)])]
      [(exact-integer? v)
       (write v op)]
      [(boolean? v)
       (display (if v "#true" "#false") op)]
      [(void? v)
       (display "#void" op)]
      [(printer-ref v #f)
       => (lambda (printer)
            (printer v op mode))]
      [(path? v)
       (cond
         [(eq? mode 'display)
          (display v op)]
         [else
          (write v op)])]
      [(struct? v)
       (define vec (struct->vector v))
       (write (if (srcloc? v)
                  'Srcloc
                  (object-name v))
              op)
       (display "(" op)
       (cond
         [(print-field-shapes-ref v #f)
          => (lambda (shapes)
               (cond
                 [(eq? shapes 'opaque)
                  (display "..." op)]
                 [else
                  (void
                   (for/fold ([did? #f]) ([i (in-range 1 (vector-length vec))]
                                          [s (in-list shapes)]
                                          #:when s)
                     (when did? (display ", " op))
                     (when (keyword? s)
                       (display (string-append "~" (keyword->immutable-string s) ": ") op))
                     (print (vector-ref vec i))
                     #t))]))]
         [else
          (for ([i (in-range 1 (vector-length vec))])
            (unless (eqv? i 1) (display ", " op))
            (print (vector-ref vec i)))])
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
       (when (mutable-hash? v)
         (display "MutableMap" op))
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
       (cond
         [(eqv? 0 (hash-count (set-ht v)))
          (when (mutable-hash? (set-ht v))
            (display "Mutable" op))
          (display "Set{}" op)]
         [else
          (when (mutable-hash? (set-ht v))
            (display "MutableSet" op))
          (display "{" op)
          (for/fold ([first? #t]) ([v (in-list (hash-map (set-ht v) (lambda (k v) k) #t))])
            (unless first? (display ", " op))
            (print v)
            #f)
          (display "}" op)])]
      [(syntax? v)
       (define s (syntax->datum v))
       (define maybe-nested? (let loop ([s s ])
                               (and (pair? s)
                                    (case (car s)
                                      [(quotes) #t]
                                      [(op s) #f]
                                      [else (ormap loop (cdr s))]))))
       (display (if maybe-nested? "'«" "'") op)
       (cond
         [(and (pair? s) (eq? 'multi (car s)))
          (write-shrubbery (cons 'top (cdr s)) op)]
         [(and (pair? s) (eq? 'group (car s)))
          (write-shrubbery (list 'top s) op)]
         [else
          (write-shrubbery s op)])
       (display (if maybe-nested? "»'" "'") op)]
      [(procedure? v)
       (define name (adjust-procedure-name (object-name v) (procedure-realm v)))
       (cond
         [name
          (display "#<function:" op)
          (display name op)
          (display ">" op)]
         [else
          (display "#<function>") op])]
      [(symbol? v)
       (cond
         [(display?)
          (display (symbol->immutable-string v))]
         [else
          (display "#'" op)
          (write-shrubbery v op)])]
      [(keyword? v)
       (cond
         [(display?)
          (display (keyword->immutable-string v))]
         [else
          (display "#'" op)
          (write-shrubbery v op)])]
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

(define-static-info-syntaxes (current-output-port current-error-port)
  (#%function-arity 6))
