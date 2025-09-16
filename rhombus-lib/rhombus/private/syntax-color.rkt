#lang racket/base

(require (for-syntax racket/base)
         racket/port
         racket/promise
         racket/runtime-path
         racket/string
         shrubbery/syntax-color
         shrubbery/variant)

(provide rhombus-lexer
         make-rhombus-lexer)

(define-runtime-path builtins.txt
  "syntax-color/builtins.txt")
(define-runtime-path keywords.txt
  "syntax-color/keywords.txt")

(define (make-identifier?-procedure path)
  (define ht-promise
    (delay/sync
     (for/hash ([str (in-list (call-with-input-file path port->lines))])
       (values str #t))))
  (lambda (tok)
    (hash-ref (force ht-promise) tok #f)))

(define builtin-identifier?
  (make-identifier?-procedure builtins.txt))
(define keyword-identifier?
  (make-identifier?-procedure keywords.txt))

(define (rhombus-semantic-type-guess tok)
  (cond
    [(keyword-identifier? tok) 'keyword]
    [(builtin-identifier? tok) 'builtin]
    [else #f]))

(define (default-semantic-type-guess tok default)
  (default tok))

(define (make-rhombus-lexer #:variant [variant default-variant]
                            #:semantic-type-guess [semantic-type-guess default-semantic-type-guess])
  (lambda (in pos status)
    (define-values (tok type paren start end backup res-status)
      (shrubbery-lexer in pos status #:variant variant))
    (define type-sym
      (and (hash? type)
           (hash-ref type 'type)))
    (define res-type
      (case type-sym
        [(symbol)
         (cond
           [(semantic-type-guess tok rhombus-semantic-type-guess)
            => (lambda (guess)
                 (hash-set type 'semantic-type-guess guess))]
           [else type])]
        [else
         type]))
    (values tok res-type paren start end backup res-status)))

(define rhombus-lexer
  (make-rhombus-lexer))

;; Run this submodule to refresh the keywords and builtins lists.
(module+ main
  (require racket/list)
  (define-runtime-module-path-index rhombus/and_meta
    '(lib "rhombus/and_meta"))
  (dynamic-require rhombus/and_meta #f)
  (define-values (vars stxs)
    (module->exports rhombus/and_meta))
  (define (operator-like? s)
    (not (regexp-match? #rx"^[a-zA-Z_]" s)))
  (define (get-syms phase+exportss [include? (λ (_sym _sym-str) #t)])
    (sort
     (remove-duplicates
      (for*/list ([phase+exports (in-list phase+exportss)]
                  [export (in-list (cdr phase+exports))]
                  #:do [(define sym (car export))
                        (define sym-str (symbol->string sym))]
                  #:unless (operator-like? sym-str)
                  #:when (include? sym sym-str))
        sym))
     symbol<?))
  (define keyword-syms
    (get-syms
     stxs
     (lambda (sym sym-str)
       (and
        ;; Most Rhombus symbols export as syntax, but treating
        ;; annotations and classes as keywords feels wrong, so exclude
        ;; any symbols starting with a capital letter.
        (regexp-match? #rx"^[^A-Z]" sym-str)
        ;; Exclude any regular values, too, so they can be included in
        ;; the builtins list instead.
        (not (with-handlers ([exn:fail? (λ (_) #f)])
               (and (dynamic-require rhombus/and_meta sym) #t)))))))
  (define builtin-syms
    (get-syms
     (append vars stxs)
     (lambda (sym _sym-str)
       (not (memq sym keyword-syms)))))
  (call-with-output-file keywords.txt
    #:exists 'truncate/replace
    (lambda (out)
      (for ([sym (in-list keyword-syms)])
        (displayln sym out))))
  (call-with-output-file builtins.txt
    #:exists 'truncate/replace
    (lambda (out)
      (for ([sym (in-list builtin-syms)])
        (displayln sym out)))))
