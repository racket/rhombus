#lang racket/base
(require racket/keyword
         racket/symbol
         "private/simple-pretty.rkt")

;; Writing a shubbery represented as an S-expression.

(provide write-shrubbery
         pretty-shrubbery)

(define rx:identifier #px"^(?:#%)?(?:\\p{L}|_)(?:\\p{L}|\\p{N}|_)*$")

(define (write-shrubbery v [op (current-output-port)]
                         #:pretty? [pretty? #f]
                         #:armor? [armor? #f]
                         #:prefer-multiline? [prefer-multiline? #f]
                         #:width [width #f])
  (cond
    [pretty?
     (define doc (pretty-shrubbery v #:armor? armor? #:prefer-multiline? prefer-multiline?))
     (render-pretty doc op #:width width)]
    [else
     (do-write-shrubbery-term v op)]))

(define (pretty-shrubbery v
                          #:armor? [armor? #f]
                          #:prefer-multiline? [prefer-multiline? #f])
  (define-values (inside inside-multi quotes? atomic?)
    (do-write-shrubbery-term v (if armor?
                                   'armor-doc
                                   (if prefer-multiline?
                                       'multi-doc
                                       'doc))))
  (if inside ; may be #f in 'multi-doc mode
      `(or ,inside ,inside-multi)
      inside-multi))

;; returns three values when `op` is a symbol:
;;   - instructions for single-line mode
;;   - instructions for multi-line mode
;;   - whether exposed quotes appear (in either mode)
;;   - whether an enclosing `|` can do without '«»' around its single-line content
(define (do-write-shrubbery-term v op)
  (let loop ([v v]
             [non-tail? #f] ; => unarmored single-line block still will needs « and »
             [head? #t]     ; => '|' doesn't need a leading newline in 'multi-doc mode
             [before-block? #f]) ; => ':' is next
    (cond
      [(list? v)
       (cond
         [(null? v)
          #;(error 'write-shubbery "unexpected ~s" v)
          (display* "#{()}" op)]
         [(eq? 'op (car v))
          (define name (cadr v))
          (cond
            [(and before-block?
                  (regexp-match? #rx"^:+$" (symbol->immutable-string name)))
             (display* (format "~a " name) op)]
            [else
             (display* name op)])]
         [(eq? 'alts (car v))
          (cond
            [(symbol? op)
             (define-values (insides insides-multi quotes?s)
               (for/lists (insides insides-multi quotes?s) ([v (in-list (cdr v))])
                 (unless (and (pair? v) (eq? (car v) 'block))
                   (error 'write-shubbery "unexpected ~s" v))
                 (define-values (sub-insides sub-insides-multi sub-quotes?s sub-atomic?s)
                   (for/lists (sub-insides sub-insides-multi sub-quotes?s sub-atomic?s) ([v (in-list (cdr v))])
                     (loop v #f #f #f)))
                 (values (if (null? (cdr v))
                             `(seq "|«»")
                             (and (all-of? sub-insides)
                                  (or (not (eq? op 'multi-doc))
                                      (and (null? (cdr sub-insides))
                                           (car sub-atomic?s)))
                                  (let ([line `(seq ,@(add-between sub-insides "; "))])
                                    (if (all-of? sub-atomic?s)
                                        `(seq "| " ,line)
                                        `(seq "|« " ,line" »")))))
                         (cond
                           [(null? (cdr v))
                            `(seq "|«»")]
                           [(eq? op 'multi-doc)
                            (define multi
                              `(seq "| " (align (seq ,@(add-between sub-insides-multi 'nl)))))
                            (if (and (pair? sub-insides)
                                     (null? (cdr sub-insides))
                                     (car sub-atomic?s))
                                `(or (seq "| " ,(car sub-insides))
                                     ,multi)
                                multi)]
                           [else
                            ;; "|" is added outside the `for/lists` loop
                            `(nest 2 (seq nl ,@(add-between sub-insides-multi (if (eq? op 'armor-doc)
                                                                                  '(seq ";" nl)
                                                                                  'nl))))])
                         (any-of? sub-quotes?s))))
             (values (and (not (eq? op 'multi-doc))
                          `(seq ,@(add-between insides " ")))
                     (if (eq? op 'multi-doc)
                         `(seq ,(if head? '(seq) 'nl) (align (seq ,@(add-between insides-multi 'nl))))
                         `(seq nl (align ,(if (eq? op 'armor-doc)
                                              `(seq "|«" ,@(add-between insides-multi `(seq nl "»" nl "|«")) nl "»")
                                              `(seq "|" ,@(add-between insides-multi `(seq nl nl "|")))))))
                     (any-of? quotes?s)
                     #f)]
            [else
             (for/fold ([first? #t]) ([v (in-list (cdr v))])
               (unless first? (display " " op))
               (display "|« " op)
               (unless (and (pair? v) (eq? (car v) 'block))
                 (error 'write-shubbery "unexpected ~s" v))
               (for/fold ([first? #t]) ([v (in-list (cdr v))])
                 (unless first? (display "; " op))
                 (loop v #f #f #f)
                 #f)
               (display " »" op)
               #f)
             (void)])]
         [(eq? 'multi (car v))
          (cond
            [(symbol? op)
             (define last-i (- (length v) 2))
             (define-values (insides insides-multi quotes?s atomic?s)
               (for/lists (insides insides-multi quotes?s atomic?s) ([v (in-list (cdr v))]
                                                                     [i (in-naturals)])
                 (loop v (not (= i last-i)) #f #f)))
             (values (and (or (not (eq? op 'multi-doc))
                              (null? insides)
                              (and (null? (cdr insides))
                                   (car insides)))
                          `(seq ,@(add-between insides "; ")))
                     (if (eq? op 'armor-doc)
                         `(seq ";«" (nest 2 (seq nl ,@(add-between insides-multi '(seq ";" nl)))) nl "»")
                         `(seq ,@(add-between insides-multi 'nl)))
                     (any-of? quotes?s)
                     #f)]
            [else
             (for/fold ([first? #t]) ([v (in-list (cdr v))])
               (unless first? (display "; " op))
               (loop v #f #f #f)
               #f)
             (void)])]
         [else
          (define armor? (eq? op 'armor-doc))
          (define mt? (null? (cdr v)))
          (define a/mt? (or armor? mt?))
          (define prefer-multi? (eq? op 'multi-doc))
          (define a/nt? (or armor? (and non-tail? (not prefer-multi?)) (not (symbol? op))))
          (define-values (align? q-open q-line-open
                                 sep sep+space block-sep+space
                                 q-line-close q-close
                                 one-line? is-quotes? use-non-tail? can-tail? wraps?
                                 no-line?)
            (case (car v)
              [(group) (values #t "" "" "" " " "" "" ""
                               #t #f #t (not non-tail?) #f
                               #f)]
              [(block) (values #f (if a/mt? ":«" ":") (if mt? ":«" (if a/nt? ":« " ": "))
                               (if armor? ";" "") "; " "; "
                               (if mt? "»" (if a/nt? " »" "")) (if a/mt? "»" #f)
                               #f #f #t #t #f
                               (and prefer-multi? (or a/mt? (pair? (cddr v)))))]
              [(parens) (values #f "(" "("
                                "," ", " ", "
                                ")" ")"
                                #f #f #f #t #t
                                #f)]
              [(brackets) (values #f "[" "["
                                  "," ", " ", "
                                  "]" "]"
                                  #f #f #f #t #t
                                  #f)]
              [(braces) (values #f "{" "{"
                                "," ", " ", "
                                "}" "}"
                                #f #f #f #t #t
                                #f)]
              [(quotes) (values #f '("'«" "'") '("'«" "'")
                                (if armor? ";" "") "; " "; "
                                '("»'" "'") '("»'" "'")
                                #f #t #t #t #t
                                (and prefer-multi? (pair? (cdr v)) (pair? (cddr v))))]
              [else (values #f #f #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f)]))
          (cond
            [q-open
             (cond
               [(symbol? op)
                (define open-length 1)
                (define-values (insides insides-semi insides-line-multi insides-multi insides-quotes?)
                  ;; add separators here, since we need to treat block items
                  ;; specially for 'group mode
                  (let inside-loop ([l (cdr v)] [first? #t])
                    (cond
                      [(null? l) (values null null null null #f)]
                      [(null? (cdr l))
                       (define-values (inside inside-multi quotes? atomic?) (loop (car l) (not can-tail?) (or wraps? (and first? head?)) #f))
                       (values (and inside
                                    (list inside))
                               (list inside-multi)
                               (list (if first?
                                         inside-multi
                                         `(nest ,open-length (seq nl ,inside-multi))))
                               (list `(nest 2 (seq nl ,inside-multi)))
                               quotes?)]
                      [else
                       (define v (car l))
                       (define next-v (cadr l))
                       (define next-is-block? (and (pair? next-v) (eq? (car next-v) 'block)))
                       (define-values (inside inside-multi inside-quotes? inside-atomic?) (loop v use-non-tail? (or wraps? (and first? head?)) next-is-block?))
                       (define-values (insides insides-semi insides-line-multi insides-multi insides-quotes?) (inside-loop (cdr l) #f))
                       (define this-sep+space (if (and (pair? next-v)
                                                       (memq (car next-v) '(block alts)))
                                                  block-sep+space
                                                  sep+space))
                       (values (and inside
                                    insides
                                    (list* inside this-sep+space insides))                               
                               (let ([semi (and inside
                                                insides-semi
                                                (list* inside this-sep+space insides-semi))])
                                 (cond
                                   [(and (null? (cddr l))
                                         (pair? (car l))
                                         (eq? (car (car l)) 'block))
                                    ;; must be a block with alts after
                                    (if semi
                                        (list `(or (seq ,@semi)
                                                   (seq ,inside-multi
                                                        ,@insides-semi)))
                                        ;; In this case, we're in 'multi-doc mode, and
                                        ;; `insides-semi` cannot be #f, since the last part
                                        ;; can use multi mode. Along those lines, it's ok to
                                        ;; use multi mode for the next-to-last element
                                        (list `(seq ,inside-multi ,@insides-semi)))]
                                   [else semi]))
                               (cons `(nest ,open-length (seq ,(if first? '(seq) 'nl)
                                                              ,inside-multi
                                                              ,sep))
                                     insides-line-multi)
                               (cons `(nest 2 (seq nl ,inside-multi ,sep))
                                     insides-multi)
                               (or inside-quotes? insides-quotes?))])))
                (define quotes? (or is-quotes?
                                    (and (not wraps?) insides-quotes?)))
                (define-values (open line-open line-close close)
                  (let ([sel (lambda (v) (if (pair? v)
                                             (if insides-quotes? (car v) (cadr v))
                                             v))])
                    (values (sel q-open) (sel q-line-open) (sel q-line-close) (sel q-close))))
                (define line
                  (and insides
                       `(seq ,line-open ,@insides ,line-close)))
                (cond
                  [(null? (cdr v)) (values line line quotes? wraps?)]
                  [else
                   (values (and (not no-line?) line)
                           (let* ([multi
                                   (cond
                                     [one-line?
                                      `(seq ,line-open ,@insides-semi ,line-close)]
                                     [else (if close
                                               `(seq ,open ,@insides-multi nl ,close)
                                               `(seq ,open ,@insides-multi))])]
                                  [multi* (if align? `(align ,multi) multi)])
                             (cond
                               [(or one-line?
                                    (and (not wraps?)
                                         (pair? (cdr insides-multi))))
                                multi*]
                               [prefer-multi?
                                (define multi**
                                  (if wraps?
                                      `(or ,multi*
                                           (align (seq ,line-open ,@insides-line-multi ,line-close)))
                                      multi*))
                                (if (and (not no-line?) line)
                                    `(or ,line ,multi**)
                                    multi**)]
                               [else
                                `(or ,line
                                     ,multi*)]))
                           quotes?
                           wraps?)])]
               [else
                (define (sel v) (if (pair? v) (car v) v))
                (display (sel q-line-open) op)
                (let v-loop ([first? #t]
                             [vs (cdr v)])
                  (unless (null? vs)
                    (define v (car vs))
                    (unless (or first?
                                (and (pair? v) (eq? (car v) 'block)))
                      (display sep+space op))
                    (define next-vs (cdr vs))
                    (define next-v (and (pair? next-vs) (car next-vs)))
                    (define next-is-block? (and (pair? next-v) (eq? (car next-v) 'block)))
                    (loop v #f #f next-is-block?)
                    (v-loop #f next-vs)))
                (display (sel q-line-close) op)])]
            [else
             (write-escaped v op)])])]
      [(symbol? v)
       (define s (symbol->immutable-string v))
       (cond
         [(regexp-match? rx:identifier s)
          (display* s op)]
         [else
          (write-escaped v op)])]
      [(keyword? v)
       (define s (keyword->immutable-string v))
       (cond
         [(regexp-match? rx:identifier s)
          (cond
            [(symbol? op)
             (twice (string-append-immutable "~" s))]
            [else
             (display "~" op)
             (display s op)])]
         [else
          (write-escaped v op)])]
      [(or (string? v)
           (bytes? v)
           (exact-integer? v)
           (and (rational? v) (exact? v)))
       (write* v op)]
      [(flonum? v)
       (cond
         [(eqv? v +inf.0) (display* "#inf" op)]
         [(eqv? v -inf.0) (display* "#neginf" op)]
         [(eqv? v +nan.0) (display* "#nan" op)]
         [else (write* v op)])]
      [(boolean? v)
       (display* (if v "#true" "#false") op)]
      [(void? v)
       (display* "#void" op)]
      [else
       (write-escaped v op)])))

(define (display* v op)
  (if (symbol? op)
      (twice (if (or (string? v)
                     (bytes? v))
                 v
                 (format "~a" v)))
      (display v op)))

(define (write* v op)
  (if (symbol? op)
      (twice (format "~s" v))
      (write v op)))

(define (write-escaped v op)
  (cond
    [(symbol? op)
     (cond
       [(keyword? v)
        (twice (format "~~#{~s}" (string->symbol (keyword->immutable-string v))))]
       [else
        (twice (format "#{~s}" v))])]
    [else
     (cond
       [(keyword? v)
        (display "~#{" op)
        (write (string->symbol (keyword->immutable-string v)) op)
        (display "}" op)]
       [else
        (display "#{" op)
        (write v op)
        (display "}" op)])]))

(define (twice v)
  (values v v #f #t))

(define (add-between l v)
  (cond
    [(null? l) l]
    [(null? (cdr l)) l]
    [else (list* (car l) v (add-between (cdr l) v))]))

(define (any-of? quotes?s)
  (for/or ([quotes? (in-list quotes?s)])
    quotes?))

(define (all-of? quotes?s)
  (for/and ([quotes? (in-list quotes?s)])
    quotes?))
