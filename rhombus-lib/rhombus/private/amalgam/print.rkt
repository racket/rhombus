#lang racket/base
(require (for-syntax racket/base ; for `default-pretty`
                     )
         racket/symbol
         racket/keyword
         shrubbery/write
         "treelist.rkt"
         "mutable-treelist.rkt"
         "provide.rkt"
         (submod "set.rkt" for-print)
         "printer-property.rkt"
         "define-arity.rkt"
         "mutability.rkt"
         "annotation-failure.rkt"
         "print-desc.rkt"
         "key-comp-property.rkt"
         "enum.rkt")

(provide (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [rhombus-print print])
                     println
                     show
                     showln)
         (for-spaces (rhombus/annot
                      rhombus/namespace)
                     PrintMode))

(module+ redirect
  (provide (struct-out racket-print-redirect)))

(module+ for-class
  (provide prop:print-field-shapes))

(module+ for-string
  (provide (rename-out [do-print print]
                       [do-display display])))

(module+ for-runtime
  (provide (rename-out [do-print print])))

(module+ for-port
  (provide do-print*
           default-pretty))

(module+ for-printable
  (provide pretty
           check-output-port
           check-mode))

(define-values (prop:print-field-shapes print-field-shapes? print-field-shapes-ref)
  (make-struct-type-property 'print-field-shapes))

(define default-pretty
  (let-syntax ([ct-gensym (lambda (stx)
                            (datum->syntax stx `(quote ,(gensym 'default-pretty))))])
    (ct-gensym)))

(define (check-output-port who op)
  (unless (output-port? op)
    (raise-annotation-failure who op "Port.Output")))

(define-simple-symbol-enum PrintMode
  text
  expr)

(define (check-mode who mode)
  (unless (PrintMode? mode)
    (raise-annotation-failure who mode "PrintMode")))

(define (do-print* who vs op mode pretty?)
  (check-output-port who op)
  (check-mode who mode)
  (cond
    [(null? vs) (void)]
    [(null? (cdr vs)) (do-print (car vs) op mode pretty?)]
    [else
     (define sep (cond
                   [(eq? pretty? default-pretty)
                    (if (current-print-as-pretty)
                        "\n"
                        " ")]
                   [pretty? "\n"]
                   [else " "]))
     (for/fold ([first? #true]) ([v (in-list vs)])
       (unless first? (display sep op))
       (do-print v op mode pretty?)
       #f)
     (void)]))

(define/arity #:name print (rhombus-print #:out [op (current-output-port)]
                                          #:mode [mode 'text]
                                          #:pretty [pretty? default-pretty]
                                          . vs)
  (do-print* who vs op mode pretty?))

(define/arity (println #:out [op (current-output-port)]
                       #:mode [mode 'text]
                       #:pretty [pretty? default-pretty]
                       . vs)
  (do-print* who vs op mode pretty?)
  (newline op))

(define/arity (show #:out [op (current-output-port)]
                    #:pretty [pretty? default-pretty]
                    . vs)
  (do-print* who vs op 'expr pretty?))

(define/arity (showln #:out [op (current-output-port)]
                      #:pretty [pretty? default-pretty]
                      . vs)
  (do-print* who vs op 'expr pretty?)
  (newline op))

(define (do-display v op)
  (do-print v op 'text))

;; Fast path for simple printing: either call `display`, `write`,
;; or `other` once, or return results of multiple calls through `concat`
(define (maybe-print-immediate v display write concat other mode op)
  (define (display?) (eq? mode 'text))
  (cond
    [(flonum? v)
     (cond
       [(eqv? v +inf.0) (display "#inf" op)]
       [(eqv? v -inf.0) (display "#neginf" op)]
       [(eqv? v +nan.0) (display "#nan" op)]
       [else (write v op)])]
    [(or (string? v)
         (bytes? v))
     (cond
       [(display?) (display v op)]
       ;; only print immutable (byte) strings as literals
       [(immutable? v) (write v op)]
       [else (other v mode op)])]
    [(char? v)
     (cond
       [(display?)
        (display v op)]
       [else
        (concat
         (display "Char" op)
         (write (string v) op))])]
    [(exact-integer? v)
     (write v op)]
    [(and (rational? v) (exact? v))
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
        (concat
         (display "Path(" op)
         (write (path->string v) op)
         (display ")" op))])]
    [(and (procedure? v)
          (not (printer-ref v #f)))
     (define name (object-name v))
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
    [(eof-object? v)
     (display "Port.eof" op)]
    [(namespace? v) (display "#<evaluator>" op)]
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
  (render-pretty doc op racket-print-redirect))

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
             (printer v mode (lambda (v #:mode [mode 'expr])
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
    ;; (byte) strings at this point are mutable
    ;; refer to `maybe-print-immediate`
    [(or (and (string? v) "String.copy(")
         (and (bytes? v) "Bytes.copy("))
     => (lambda (pre)
          (fresh-ref
           v
           (lambda ()
             (pretty-listlike
              (pretty-text pre)
              (list (pretty-write v))
              (pretty-text ")")))))]
    [(treelist? v)
     (pretty-listlike
      (pretty-text "[")
      (for/list ([e (in-treelist v)])
        (print e))
      (pretty-text "]"))]
    [(mutable-treelist? v)
     (fresh-ref
      v
      (lambda ()
        (pretty-listlike
         (pretty-text "MutableList[")
         (for/list ([e (in-mutable-treelist v)])
           (print e))
         (pretty-text "]"))))]
    [(list? v)
     (pretty-listlike
      (pretty-text "PairList[")
      (for/list ([e (in-list v)])
        (print e))
      (pretty-text "]"))]
    [(pair? v)
     (pretty-listlike
      (pretty-text "Pair(")
      (list (print (car v))
            (print (cdr v)))
      (pretty-text ")"))]
    [(vector? v)
     (define (print-mutable-array)
       (pretty-listlike
        (pretty-text "Array(")
        (for/list ([e (in-vector v)])
          (print e))
        (pretty-text ")")))
     (if (mutable-vector? v)
         (fresh-ref v print-mutable-array)
         (pretty-listlike
          (pretty-text "Array.snapshot(")
          (list (print-mutable-array))
          (pretty-text ")")))]
    [(box? v)
     (define (print-mutable-box)
       (pretty-listlike
        (pretty-text "Box(")
        (list (print (unbox v)))
        (pretty-text ")")))
     (if (mutable-box? v)
         (fresh-ref v print-mutable-box)
         (pretty-listlike
          (pretty-text "Box.snapshot(")
          (list (print-mutable-box))
          (pretty-text ")")))]
    [(hash? v)
     (fresh-ref
      #:when (mutable-hash? v)
      v
      (lambda ()
        (pretty-listlike
         (pretty-text (cond
                        [(mutable-hash? v)
                         (string-append
                          (if (hash-ephemeron? v)
                              "Weak"
                              "")
                          (cond
                            [(hash-eq? v) "MutableMap.by(===){"]
                            [(hash-eqv? v) "MutableMap.by(is_same_number_or_object){"]
                            [(custom-map-ref v #f)
                             => (lambda (cm) (format "MutableMap.by(~a){" (custom-map-name cm)))]
                            [(hash-equal? v) "MutableMap.by(is_now){"]
                            [else "MutableMap{"]))]
                        [(hash-eq? v) "Map.by(===){"]
                        [(custom-map-ref v #f)
                         => (lambda (cm) (format "Map.by(~a){" (custom-map-name cm)))]
                        [(hash-equal? v) "Map.by(is_now){"]
                        [(hash-eqv? v) "Map.by(is_same_number_or_object){"]
                        [else "{"]))
         (for/list ([k+v (in-list (hash->list v #t))])
           (define k (car k+v))
           (define v (cdr k+v))
           (pretty-blocklike (print k) (print v)))
         (pretty-text "}"))))]
    [(set? v)
     (fresh-ref
      #:when (mutable-set? v)
      v
      (lambda ()
        (define elems (for/list ([v (in-list (set->list v #t))])
                        (print v)))
        (pretty-listlike
         (pretty-text (cond
                        [(mutable-set? v)
                         (string-append
                          (if (hash-weak? (set-ht v))
                              "Weak"
                              "")
                          (cond
                            [(hash-eq? (set-ht v)) "MutableSet.by(===){"]
                            [(custom-map-ref (set-ht v) #f)
                             => (lambda (cm) (format "MutableSet.by(~a){" (custom-map-name cm)))]
                            [(hash-equal? (set-ht v)) "MutableSet.by(is_now){"]
                            [(hash-eqv? (set-ht v)) "MutableSet.by(is_same_number_or_object){"]
                            [else "MutableSet{"]))]
                        [(hash-eq? (set-ht v)) "Set.by(===){"]
                        [(custom-map-ref (set-ht v) #f)
                         => (lambda (cm) (format "Set.by(~a){" (custom-map-name cm)))]
                        [(hash-equal? (set-ht v)) "Set.by(is_now){"]
                        [(hash-eqv? (set-ht v)) "Set.by(is_same_number_or_object){"]
                        [(null? elems) "Set{"]
                        [else "{"]))
         elems
         (pretty-text "}"))))]
    [(syntax? v)
     (define s (syntax->datum v))
     (define qs
       (cond
         [(and (pair? s) (eq? 'multi (car s)))
          (if (display?)
              s
              (cons 'quotes (cdr s)))]
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
        (display "#{" rop)
        (unless (number? v)
          (display "'" rop))
        (racket-print (racket-print-redirect v) rop 1)
        (display "}" rop)
        (pretty-text (get-output-bytes rop))])]))

(define (racket-print v op mode)
  (print v op mode))

(struct racket-print-redirect (val)
  #:authentic
  #:sealed
  #:property prop:custom-write
  (lambda (r op mode)
    (case mode
      [(0 1)
       (racket-print (racket-print-redirect-val r) op mode)]
      [(#f)
       (write (racket-print-redirect-val r) op)]
      [else
       (display (racket-print-redirect-val r) op)])))
