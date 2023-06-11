#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "pack.rkt")
         syntax/parse/pre
         syntax/strip-context
         racket/syntax-srcloc
         shrubbery/property
         shrubbery/print
         "provide.rkt"
         "expression.rkt"
         (submod "annotation.rkt" for-class)
         "pack.rkt"
         "realm.rkt"
         "name-root.rkt"
         "tag.rkt"
         "dot-parse.rkt"
         "dotted-sequence.rkt"
         "static-info.rkt"
         "define-arity.rkt"
         "srcloc-span.rkt"
         (submod "dot.rkt" for-dot-provider)
         (submod "srcloc-object.rkt" for-static-info)
         (submod "string.rkt" static-infos))

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Syntax)
         (for-spaces (rhombus/annot)
                     Term
                     Group
                     Identifier
                     Operator
                     Name
                     IdentifierName))

(module+ for-builtin
  (provide syntax-method-table))

(module+ for-quasiquote
  (provide (for-syntax syntax-static-infos)))

(define-for-syntax syntax-static-infos
  #'((#%dot-provider syntax-instance)))

(define-annotation-syntax Syntax
  (identifier-annotation #'syntax? syntax-static-infos))

(define-annotation-syntax Identifier
  (identifier-annotation #'is-identifier? syntax-static-infos))
(define (is-identifier? s)
  (and (syntax? s)
       (identifier? (unpack-term s #f #f))))

(define-annotation-syntax Operator
  (identifier-annotation #'is-operator? syntax-static-infos))
(define (is-operator? s)
  (and (syntax? s)
       (let ([t (unpack-term s #f #f)])
         (syntax-parse t
           #:datum-literals (op)
           [(op _) #t]
           [_ #f]))))

(define-annotation-syntax Name
  (identifier-annotation #'syntax-name? syntax-static-infos))
(define (syntax-name? s)
  (and (syntax? s)
       (let ([t (unpack-term s #f #f)])
         (or (identifier? t)
             (if t
                 (syntax-parse t
                   #:datum-literals (op)
                   [(op _) #t]
                   [_ #f])
                 (let ([t (unpack-group s #f #f)])
                   (and t
                        (syntax-parse t
                          #:datum-literals (group)
                          [(group _::dotted-operator-or-identifier-sequence) #t]
                          [_ #f]))))))))

(define-annotation-syntax IdentifierName
  (identifier-annotation #'syntax-identifier-name? syntax-static-infos))
(define (syntax-identifier-name? s)
  (and (syntax? s)
       (let ([t (unpack-term s #f #f)])
         (or (identifier? t)
             (and (not t)
                  (let ([t (unpack-group s #f #f)])
                    (and t
                         (syntax-parse t
                           #:datum-literals (group)
                           [(group _::dotted-identifier-sequence) #t]
                           [_ #f]))))))))

(define-annotation-syntax Term
  (identifier-annotation #'syntax-term? syntax-static-infos))
(define (syntax-term? s)
  (and (syntax? s)
       (unpack-term s #f #f)
       #t))

(define-annotation-syntax Group
  (identifier-annotation #'syntax-group? syntax-static-infos))
(define (syntax-group? s)
  (and (syntax? s)
       (unpack-group s #f #f)
       #t))

(define-name-root Syntax
  #:fields
  (literal
   literal_group
   make
   make_op
   make_group
   make_sequence
   make_id
   unwrap
   unwrap_op
   unwrap_group
   unwrap_sequence
   strip_scopes
   replace_scopes
   relocate
   relocate_span
   to_code_string
   [srcloc get-srcloc]))

(define-syntax literal
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(_ ((~or parens quotes) (group term)) . tail)
        ;; Note: discarding group properties in this case
        (values #'(quote-syntax term) #'tail)]
       [(_ (~and ((~or parens quotes) . _) gs) . tail)
        (values #`(quote-syntax #,(pack-tagged-multi #'gs)) #'tail)]))))

(define-syntax literal_group
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(_ ((~or parens quotes)) . tail)
        (values #`(quote-syntax #,(pack-multi '())) #'tail)]
       [(_ ((~or parens quotes) g) . tail)
        (values #'(quote-syntax g) #'tail)]))))

;; ----------------------------------------

(define (extract-ctx who ctx-stx)
  (and ctx-stx
       (begin
         (unless (syntax? ctx-stx)
           (raise-argument-error* who rhombus-realm "maybe(Syntax)" ctx-stx))
         (unpack-term ctx-stx who #f))))

(define (do-make who v ctx-stx tail? group?)
  ;; assume that any syntax objects are well-formed, while list structure
  ;; needs to be validated
  (define ctx-stx-t (extract-ctx who ctx-stx))
  (define (invalid)
    (raise-arguments-error* who rhombus-realm
                            (if group?
                                "invalid as a shrubbery group representation"
                                (if tail?
                                    "invalid as a shrubbery term representation"
                                    "invalid as a shrubbery non-tail term representation"))
                            "value" v))
  (define (group-loop l)
    (for/list ([e (in-list l)])
      (group e)))
  (define (group e)
    (cond
      [(and (pair? e)
            (list? e))
       (define head-stx (car e))
       (define head (if (syntax? head-stx) (syntax-e head-stx) head-stx))
       (if (eq? head 'group)
           (cons head-stx
                 (let l-loop ([es (cdr e)])
                   (cond
                     [(null? es) null]
                     [else
                      (define ds (cdr es))
                      (cons (loop (car es) (null? ds))
                            (l-loop ds))])))
           (invalid))]
      [(syntax? e)
       (or (unpack-group e #f #f)
           (invalid))]
      [else (invalid)]))
  (define (loop v tail?)
    (cond
      [(null? v) (invalid)]
      [(list? v)
       (define head-stx (car v))
       (define head (if (syntax? head-stx) (syntax-e head-stx) head-stx))
       (case head
         [(parens brackets braces quotes)
          (cons head-stx (group-loop (cdr v)))]
         [(block)
          (if tail?
              (cons head-stx (group-loop (cdr v)))
              (invalid))]
         [(alts)
          (if tail?
              (cons head-stx
                    (for/list ([e (in-list (cdr v))])
                      (cond
                        [(and (pair? e)
                              (list? e))
                         (define head-stx (car e))
                         (define head (if (syntax? head-stx) (syntax-e head-stx) head-stx))
                         (if (eq? head 'block)
                             (loop e #t)
                             (invalid))]
                        [(syntax? e)
                         (define u (unpack-term e #f #f))
                         (define d (and u (syntax-e u)))
                         (or (and d
                                  (eq? 'block (syntax-e (car d)))
                                  u)
                             (invalid))]
                        [else (invalid)])))
              (invalid))]
         [(op)
          (if (and (pair? (cdr v))
                   (null? (cddr v))
                   (let ([op (cadr v)])
                     (or (symbol? op)
                         (identifier? op))))
              v
              (invalid))]
         [else (invalid)])]
      [(pair? v) (invalid)]
      [(syntax? v) (or (unpack-term v #f #f)
                       (invalid))]
      [else v]))
  (datum->syntax ctx-stx-t (if group? (group v) (loop v tail?))))

(define/arity (make v [ctx-stx #f])
  #:static-infos ((#%call-result #,syntax-static-infos))
  (do-make 'Syntax.make v ctx-stx #t #f))

(define/arity (make_op v [ctx-stx #f])
  #:static-infos ((#%call-result #,syntax-static-infos))
  (unless (symbol? v)
    (raise-argument-error* 'Syntax.make_op "Symbol" v))
  (do-make 'Syntax.make (list 'op v) ctx-stx #t #f))

(define/arity (make_group v [ctx-stx #f])
  #:static-infos ((#%call-result #,syntax-static-infos))
  (unless (and (pair? v)
               (list? v))
    (raise-argument-error* 'Syntax.make_group rhombus-realm "NonemptyList" v))
  (datum->syntax #f (cons group-tag (let loop ([es v])
                                      (cond
                                        [(null? es) null]
                                        [else
                                         (define ds (cdr es))
                                         (cons (do-make 'Syntax.make_group (car es) ctx-stx (null? ds) #f)
                                               (loop ds))])))))

(define/arity (make_sequence v [ctx-stx #f])
  #:static-infos ((#%call-result #,syntax-static-infos))
  (unless (list? v) (raise-argument-error* 'Syntax.make_sequence rhombus-realm "List" v))
  (pack-multi (for/list ([e (in-list v)])
                (do-make 'Syntax.make_sequence e ctx-stx #t #t))))

(define/arity (make_id str [ctx #f])
  #:static-infos ((#%call-result #,syntax-static-infos))
  (unless (string? str)
    (raise-argument-error* 'Syntax.make_id rhombus-realm "StringView" str))
  (datum->syntax (extract-ctx 'Syntax.make_id ctx)
                 (string->symbol str)))

(define/arity (unwrap v)
  (cond
    [(not (syntax? v))
     (raise-argument-error* 'Syntax.unwrap rhombus-realm "Syntax" v)]
    [else
     (define unpacked (unpack-term v 'Syntax.unwrap #f))
     (define u (syntax-e unpacked))
     (cond
       [(and (pair? u)
             (eq? (syntax-e (car u)) 'parsed))
        v]
       [else
        (if (and (pair? u)
                 (not (list? u)))
            (syntax->list unpacked)
            u)])]))

(define/arity (unwrap_op v)
  (cond
    [(not (syntax? v))
     (raise-argument-error* 'Syntax.unwrap_up rhombus-realm "Syntax" v)]
    [else
     (syntax-parse (unpack-term v 'Syntax.unwrap #f)
       #:datum-literals (op)
       [(op o) (syntax-e #'o)]
       [else
        (raise-arguments-error* 'Syntax.unwrap_up rhombus-realm
                                "syntax object does not have just an operator"
                                "syntax object" v)])]))

(define/arity (unwrap_group v)
  (cond
    [(not (syntax? v))
     (raise-argument-error* 'Syntax.unwrap_group rhombus-realm "Syntax" v)]
    [else
     (syntax->list (unpack-tail v 'Syntax.unwrap_group #f))]))
  
(define/arity (unwrap_sequence v)
  (cond
    [(not (syntax? v))
     (raise-argument-error* 'Syntax.unwrap_sequence rhombus-realm "Syntax" v)]
    [else
     (syntax->list (unpack-multi-tail v 'Syntax.unwrap_sequence #f))]))

(define/arity (strip_scopes v)
  #:static-infos ((#%call-result #,syntax-static-infos))
  (cond
    [(not (syntax? v))
     (raise-argument-error* 'Syntax.strip_scopes rhombus-realm "Syntax" v)]
    [else
     (strip-context v)]))

(define/arity (replace_scopes v ctx)
  #:static-infos ((#%call-result #,syntax-static-infos))
  (unless (syntax? v)
    (raise-argument-error* 'Syntax.replace_scopes rhombus-realm "Syntax" v))
  (unless (syntax? ctx)
    (raise-argument-error* 'Syntax.replace_scopes rhombus-realm "Syntax" ctx))
  (replace-context ctx v))

(define replace_scopes_method
  (lambda (v)
    (let ([replace_scopes (lambda (ctx)
                            (replace_scopes v ctx))])
      replace_scopes)))

(define/arity (relocate stx ctx-stx-in)
  #:static-infos ((#%call-result #,syntax-static-infos))
  (unless (syntax? stx) (raise-argument-error* 'Syntax.relocate rhombus-realm "Syntax" stx))
  (unless (syntax? ctx-stx-in) (raise-argument-error* 'Syntax.relocate rhombus-realm "Syntax" ctx-stx-in))
  (relocate* stx ctx-stx-in))

(define relocate_method
  (lambda (stx)
    (let ([relocate (lambda (ctx-stx)
                      (relocate stx ctx-stx))])
      relocate)))

(define/arity (relocate_span stx ctx-stxes-in)
  #:static-infos ((#%call-result #,syntax-static-infos))
  (unless (syntax? stx) (raise-argument-error* 'Syntax.relocate_span rhombus-realm "Syntax" stx))
  (relocate-span stx ctx-stxes-in))

(define relocate_span_method
  (lambda (stx)
    (let ([relocate_span (lambda (ctx-stxes)
                           (relocate_span stx ctx-stxes))])
      relocate_span)))

(define/arity (to_code_string stx)
  #:static-infos ((#%call-result #,string-static-infos))  
  (unless (syntax? stx) (raise-argument-error* 'Syntax.to_code_string rhombus-realm "Syntax" stx))
  (string->immutable-string (shrubbery-syntax->string stx)))

(define syntax-method-table
  (hash 'unwrap (method1 unwrap)
        'unwrap_op (method1 unwrap_op)
        'unwrap_group (method1 unwrap_group)
        'unwrap_sequence (method1 unwrap_sequence)
        'strip_scopes (method1 strip_scopes)
        'replace_scopes replace_scopes_method
        'relocate relocate_method
        'relocate_span relocate_span_method
        'srcloc (method1 syntax-srcloc)
        'to_code_string (method1 to_code_string)))

(define-syntax syntax-instance
  (dot-provider
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(unwrap) (0ary #'unwrap)]
        [(unwrap_op) (0ary #'unwrap_op)]
        [(unwrap_group) (0ary #'unwrap_group)]
        [(unwrap_sequence) (0ary #'unwrap_sequence)]
        [(strip_scopes) (0ary #'strip_scopes)]
        [(replace_scopes) (nary #'replace_scopes 2 #'replace_scopes)]
        [(relocate) (nary #'relocate_method 2 #'relocate)]
        [(relocate_span) (nary #'relocate_span_method 2 #'relocate_span)]
        [(srcloc) (0ary #'get-srcloc)]
        [(to_code_string) (0ary #'to_code_string)]
        [else (fail-k)])))))

(define get-srcloc
  (let ([srcloc
         (lambda (v)
           (cond
             [(syntax? v)
              (define u (unpack-term v 'Syntax.srcloc #f))
              (syntax-srcloc u)]
             [else
              (raise-argument-error* 'Syntax.srcloc "Syntax" v)]))])
    srcloc))
