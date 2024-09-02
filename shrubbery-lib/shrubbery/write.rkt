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
                         #:width [width #f])
  (cond
    [pretty?
     (define doc (pretty-shrubbery v #:armor? armor?))
     (render-pretty doc op #:width width)]
    [else
     (do-write-shrubbery-term v op)]))

(define (pretty-shrubbery v #:armor? [armor? #f])
  (define-values (inside inside-multi quotes? atomic?)
    (do-write-shrubbery-term v (if armor?
                                   'armor-doc
                                   'doc)))
  `(or ,inside ,inside-multi))

;; returns three values when `op` is a symbol:
;;   - instructions for single-line mode
;;   - instructions for multi-line mode
;;   - whether exposed quotes appear (in either mode)
;;   - whether an enclosing `|` can do without '«»' around its single-line content
(define (do-write-shrubbery-term v op)
  (let loop ([v v]
             [non-tail? #f]) ; => unarmored single-line block still will needs « and »
    (cond
      [(list? v)
       (cond
         [(null? v)
          #;(error 'write-shubbery "unexpected ~s" v)
          (display* "#{()}" op)]
         [(eq? 'op (car v))
          (display* (cadr v) op)]
         [(eq? 'alts (car v))
          (cond
            [(symbol? op)
             (define-values (insides insides-multi quotes?s)
               (for/lists (insides insides-multi quotes?s) ([v (in-list (cdr v))])
                 (unless (and (pair? v) (eq? (car v) 'block))
                   (error 'write-shubbery "unexpected ~s" v))
                 (define-values (sub-insides sub-insides-multi sub-quotes?s sub-atomic?s)
                   (for/lists (sub-insides sub-insides-multi sub-quotes?s sub-atomic?s) ([v (in-list (cdr v))])
                     (loop v #f)))
                 (values (let ([line `(seq ,@(add-between sub-insides "; "))])
                           (if (all-of? sub-atomic?s)
                               `(seq "| " ,line)
                               `(seq "|« " ,line" »")))
                         `(nest 2 (seq nl ,@(add-between sub-insides-multi (if (eq? op 'armor-doc)
                                                                               '(seq ";" nl)
                                                                               'nl))))
                         (any-of? sub-quotes?s))))
             (values `(seq ,@(add-between insides " "))
                     `(seq nl (align ,(if (eq? op 'armor-doc)
                                          `(seq ,"|«" ,@(add-between insides-multi `(seq nl "»" nl "|«")) nl "»")
                                          `(seq ,"|" ,@(add-between insides-multi `(seq nl nl "|"))))))
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
                 (loop v #f)
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
                 (loop v (not (= i last-i)))))
             (values `(seq ,@(add-between insides "; "))
                     (if (eq? op 'armor-doc)
                         `(seq ";«" (nest 2 (seq nl ,@(add-between insides-multi '(seq ";" nl)))) nl "»")
                         `(seq ,@(add-between insides-multi 'nl)))
                     (any-of? quotes?s)
                     #f)]
            [else
             (for/fold ([first? #t]) ([v (in-list (cdr v))])
               (unless first? (display "; " op))
               (loop v #f)
               #f)
             (void)])]
         [else
          (define armor? (eq? op 'armor-doc))
          (define mt? (null? (cdr v)))
          (define a/mt? (or armor? mt?))
          (define a/nt? (or armor? non-tail? (not (symbol? op))))
          (define-values (align? q-open q-line-open
                                 sep sep+space block-sep+space
                                 q-line-close q-close
                                 one-line? is-quotes? use-non-tail? can-tail? wraps?)
            (case (car v)
              [(group) (values #t "" "" "" " " "" "" ""
                               #t #f #t (not non-tail?) #f)]
              [(block) (values #f (if a/mt? ":«" ":") (if mt? ":«" (if a/nt? ":« " ": "))
                               (if armor? ";" "") "; " "; "
                               (if mt? "»" (if a/nt? " »" "")) (if a/mt? "»" #f)
                               #f #f #t #t #f)]
              [(parens) (values #f "(" "("
                                "," ", " ", "
                                ")" ")"
                                #f #f #f #t #t)]
              [(brackets) (values #f "[" "["
                                  "," ", " ", "
                                  "]" "]"
                                  #f #f #f #t #t)]
              [(braces) (values #f "{" "{"
                                "," ", " ", "
                                "}" "}"
                                #f #f #f #t #t)]
              [(quotes) (values #f '("'«" "'") '("'«" "'")
                                (if armor? ";" "") "; " "; "
                                '("»'" "'") '("»'" "'")
                                #f #t #t #t #t)]
              [else (values #f #f #f #f #f #f #f #f
                            #f #f #f #f #f)]))
          (cond
            [q-open
             (cond
               [(symbol? op)
                (define-values (insides insides-semi insides-multi insides-quotes?)
                  ;; add separators here, since we need to treat block items
                  ;; specially for 'group mode
                  (let inside-loop ([l (cdr v)])
                    (cond
                      [(null? l) (values null null null #f)]
                      [(null? (cdr l))
                       (define-values (inside inside-multi quotes? atomic?) (loop (car l) (not can-tail?)))
                       (values (list inside)
                               (list inside-multi)
                               (list `(nest 2 (seq nl ,inside-multi)))
                               quotes?)]
                      [else
                       (define v (car l))
                       (define next-v (cadr l))
                       (define-values (inside inside-multi inside-quotes? inside-atomic?) (loop v use-non-tail?))
                       (define-values (insides insides-semi insides-multi insides-quotes?) (inside-loop (cdr l)))
                       (define this-sep+space (if (and (pair? next-v) (eq? (car next-v) 'block))
                                                  block-sep+space
                                                  sep+space))
                       (values (list* inside this-sep+space insides)
                               (let ([semi (list* inside this-sep+space insides-semi)])
                                 (cond
                                   [(and (null? (cddr l))
                                         (pair? (car l))
                                         (eq? (car (car l)) 'block))
                                    ;; must be a block with alts after
                                    (list `(or (seq ,@semi)
                                               (seq ,inside-multi
                                                    ,@insides-semi)))]
                                   [else semi]))
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
                  `(seq ,line-open ,@insides ,line-close))
                (cond
                  [(null? (cdr v)) (values line line quotes? wraps?)]
                  [else
                   (values line
                           (let ([multi
                                  (cond
                                    [one-line? `(seq ,line-open ,@insides-semi ,line-close)]
                                    [else (if close
                                              `(seq ,open ,@insides-multi nl ,close)
                                              `(seq ,open ,@insides-multi))])])
                             `(or ,line
                                  ,(if align? `(align ,multi) multi)))
                           quotes?
                           wraps?)])]
               [else
                (define (sel v) (if (pair? v) (car v) v))
                (display (sel q-line-open) op)
                (for/fold ([first? #t]) ([v (in-list (cdr v))])
                  (unless (or first?
                              (and (pair? v) (eq? (car v) 'block)))
                    (display sep+space op))
                  (loop v #f)
                  #f)
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
     (twice (format "#{~s}" v))]
    [else
     (display "#{" op)
     (write v op)
     (display "}" op)]))

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
