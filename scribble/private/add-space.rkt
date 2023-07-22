#lang racket/base
(require racket/symbol
         racket/keyword)

(provide full-space-names
         add-space)

(define (full-space-names space-name)
  (cond
    [(keyword? space-name)
     (full-space-names (string->symbol (keyword->immutable-string space-name)))]
    [(and (pair? space-name)
          (list? space-name))
     space-name]
    [else
     (case space-name
       [(#f) '(rhombus/defn rhombus/decl #f)]
       [(var datum value result hide) (list space-name)]
       [(expr) (list #f)]
       [else
        (unless (symbol? space-name)
          (raise-arguments-error 'normalize_space_name "bad space name"
                                 "given" space-name))
        (define str (symbol->immutable-string space-name))
        (list
         (if (regexp-match? #rx"/" str)
             space-name
             (string->symbol (string-append "rhombus/" (symbol->string space-name)))))])]))
    
(define (add-space stx space-name/full)
  (cond
    [(eq? space-name/full 'hide)
     (quote-syntax never-bound)]
    [else
     (define space
       (case space-name/full
         [(#f var value datum expr) #f]
         [else space-name/full]))
     (if space
         ((make-interned-syntax-introducer space) stx 'add)
         stx)]))
