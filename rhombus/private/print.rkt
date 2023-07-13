#lang racket/base
(require (for-syntax racket/base)
         racket/symbol
         racket/keyword
         racket/unsafe/undefined
         shrubbery/write
         "provide.rkt"
         (submod "set.rkt" for-ref)
         "adjust-name.rkt"
         "printer-property.rkt"
         "define-arity.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         "expression.rkt"
         "mutability.rkt"
         "realm.rkt"
         "print-desc.rkt")

(provide (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [rhombus-print print])
                     println))

(module+ redirect
  (provide (struct-out racket-print-redirect)))

(module+ for-class
  (provide prop:print-field-shapes))

(module+ for-string
  (provide (rename-out [do-print print]
                       [do-display display])))

(module+ for-runtime
  (provide (rename-out [do-print print])))

(module+ for-printable
  (provide pretty
           check-output-port
           check-mode))

(define-values (prop:print-field-shapes print-field-shapes? print-field-shapes-ref)
  (make-struct-type-property 'print-field-shapes))

(define default-pretty
  (let-syntax ([ct-gensym (lambda (stx)
                            (datum->syntax stx `(quote ,(gensym))))])
    (ct-gensym)))

(define (check-output-port who op)
  (unless (output-port? op)
    (raise-argument-error* who rhombus-realm "Port.Output" op)))

(define (check-mode who mode)
  (unless (or (eq? mode 'expr)
              (eq? mode 'text))
    (raise-argument-error* who rhombus-realm "#'text || #'expr" mode)))
  
(define/arity #:name print (rhombus-print v [op (current-output-port)]
                                          #:mode [mode 'text]
                                          #:pretty [pretty? default-pretty])
  (check-output-port 'print op)
  (check-mode 'print mode)
  (do-print v op mode pretty?))

(define/arity (println v [op (current-output-port)]
                       #:mode [mode 'text]
                       #:pretty [pretty? default-pretty])
  (check-output-port 'println op)
  (check-mode 'println mode)
  (do-print v op mode pretty?)
  (newline op))

(define (do-display v op)
  (do-print v op 'text))

;; Fast path for simple printing: either call `display`, `write`,
;; of `other` once, or return results of multiple calls through `concat`
(define (maybe-print-immediate v display write concat other mode op)
  (define (display?) (eq? mode 'text))
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
    [(path? v)
     (cond
       [(display?)
        (display v op)]
       [else
        (write v op)])]
    [(procedure? v)
     (define name (adjust-procedure-name (object-name v) (procedure-realm v)))
     (cond
       [name
        (concat
         (display "#<function:" op)
         (display name op)
         (display ">" op))]
       [else
        (display "#<function>" op)])]
    [(symbol? v)
     (cond
       [(display?)
        (display (symbol->immutable-string v) op)]
       [else
        (concat
         (display "#'" op)
         (write-shrubbery* v display write op))])]
    [(keyword? v)
     (cond
       [(display?)
        (display (keyword->immutable-string v) op)]
       [else
        (concat
         (display "#'" op)
         (write-shrubbery* v display write op))])]
    [(and (identifier? v)
          (display?))
     (display (syntax->datum v) op)]
    [else (other v mode op)]))

(define (write-shrubbery* v use-display use-write op)
  (cond
    [(and (eq? use-display display)
          (eq? use-write write))
     (write-shrubbery v op)]
    [else
     (define s-op (open-output-string))
     (write-shrubbery v s-op)
     (use-display (get-output-string s-op) op)]))

(define (do-print v op [mode 'expr] [pretty? default-pretty])
  (maybe-print-immediate v display write void
                         (if (eq? pretty? default-pretty)
                             print-other
                             (if pretty?
                                 print-other-pretty
                                 print-other-nonpretty))
                         mode op))

(define (print-other-pretty v mode op)
  (parameterize ([current-print-as-pretty #t])
    (print-other v mode op)))

(define (print-other-nonpretty v mode op)
  (parameterize ([current-print-as-pretty #f])
    (print-other v mode op)))

(define (print-other v mode op)
  (define doc (pretty v mode (make-hasheq)))
  (render-pretty doc op))

(define (pretty v mode ht)
  (maybe-print-immediate v pretty-display pretty-write pretty-concat pretty-other mode ht))

(define (pretty-other v mode ht)
  (define (display?) (eq? mode 'text))
  (define (print v) (pretty v 'expr ht))
  (define (fresh-ref #:when [when? #t] v thunk)
    (cond
      [when?
       (define v-ref (pretty-ref #f))
       (hash-set! ht v v-ref)
       (set-pretty-ref-doc! v-ref (thunk))
       v-ref]
      [else (thunk)]))
  (cond
    [(hash-ref ht v #f)
     => (lambda (ref) ref)]
    [(printer-ref v #f)
     => (lambda (printer)
          (fresh-ref
           v
           (lambda ()
             (printer v mode (lambda (v [mode 'expr])
                               (check-mode 'describe_recur mode)
                               (PrintDesc (pretty v mode ht)))))))]
    [(struct? v)
     (define vec (struct->vector v))
     (fresh-ref
      v
      (lambda ()
        (pretty-listlike
         (pretty-concat
          (pretty-write (if (srcloc? v)
                            'Srcloc
                            (object-name v)))
          (pretty-text "("))
         (cond
           [(print-field-shapes-ref v #f)
            => (lambda (shapes)
                 (cond
                   [(eq? shapes 'opaque)
                    (list (pretty-text "..."))]
                   [else
                    (for/list ([i (in-range 1 (vector-length vec))]
                               [s (in-list shapes)]
                               #:when s)
                      (if (keyword? s)
                          (pretty-blocklike (pretty-text (string-append "~" (keyword->immutable-string s)))
                                            (print (vector-ref vec i)))
                          (print (vector-ref vec i))))]))]
           [else
            (for/list ([i (in-range 1 (vector-length vec))])
              (print (vector-ref vec i)))])
         (pretty-text ")"))))]
    [(list? v)
     (pretty-listlike
      (pretty-text "[")
      (for/list ([e (in-list v)])
        (print e))
      (pretty-text "]"))]
    [(pair? v)
     (pretty-listlike
      (pretty-text "cons(")
      (list
       (print (car v))
       (print (cdr v)))
      (pretty-text ")"))]
    [(vector? v)
     (pretty-listlike
      (pretty-text "Array(")
      (for/list ([e (in-vector v)])
        (print e))
      (pretty-text ")"))]
    [(hash? v)
     (fresh-ref
      #:when (mutable-hash? v)
      v
      (lambda ()
        (pretty-listlike
         (pretty-text (if (mutable-hash? v)
                          "MutableMap{"
                          "{"))
         (for/list ([k+v (hash-map v cons #t)])
           (define k (car k+v))
           (define v (cdr k+v))
           (pretty-blocklike (print k) (print v)))
         (pretty-text "}"))))]
    [(set? v)
     (fresh-ref
      #:when (mutable-hash? (set-ht v))
      v
      (lambda ()
        (cond
          [(eqv? 0 (hash-count (set-ht v)))
           (if (mutable-hash? (set-ht v))
               (pretty-text "MutableSet{}")
               (pretty-text "Set{}"))]
          [else
           (pretty-listlike
            (pretty-text (if (mutable-hash? (set-ht v))
                             "MutableSet{"
                             "{"))
            (for/list ([v (in-list (hash-map (set-ht v) (lambda (k v) k) #t))])
              (print v))
            (pretty-text "}"))])))]
    [(syntax? v)
     (define s (syntax->datum v))
     (define qs
       (cond
         [(and (pair? s) (eq? 'multi (car s)))
          (cons (if (display?) 'top 'quotes)
                (cdr s))]
         [(and (pair? s) (eq? 'group (car s)))
          (if (display?)
              s
              (list 'quotes s))]
         [else (if (display?)
                   s
                   (list 'quotes (list 'group s)))]))
     (pretty-shrubbery qs #:armor? #t)]
    [else
     (cond
       [(display?)
        (pretty-display v)]
       [else
        (define rop (open-output-bytes))
        (display "#{'" rop)
        (racket-print (racket-print-redirect v) rop 1)
        (display "}" rop)
        (pretty-text (get-output-bytes rop))])]))

(define (racket-print v op mode)
  (print v op mode))

(struct racket-print-redirect (val)
  #:property prop:custom-write
  (lambda (r op mode)
    (racket-print (racket-print-redirect-val r) op mode)))
