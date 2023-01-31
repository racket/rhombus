#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "pack.rkt")
         syntax/parse/pre
         syntax/strip-context
         racket/syntax-srcloc
         shrubbery/property
         "provide.rkt"
         "expression.rkt"
         (submod "annotation.rkt" for-class)
         "pack.rkt"
         "realm.rkt"
         "name-root.rkt"
         "tag.rkt"
         "dot-parse.rkt"
         "static-info.rkt"
         "define-arity.rkt"
         (submod "dot.rkt" for-dot-provider)
         (submod "srcloc-object.rkt" for-static-info))

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Syntax)
         (for-space rhombus/annot
                    Identifier
                    Operator))

(module+ for-builtin
  (provide syntax-method-table))

(module+ for-quasiquote
  (provide (for-syntax syntax-static-infos)))

(define-for-syntax syntax-static-infos
  #'((#%dot-provider syntax-instance)))

(define-annotation-syntax Syntax
  (identifier-annotation #'syntax? syntax-static-infos))

(define-annotation-syntax Identifier
  (identifier-annotation #'identifier? syntax-static-infos))

(define-annotation-syntax Operator
  (identifier-annotation #'is-operator? syntax-static-infos))
(define (is-operator? s)
  (and (syntax? s)
       (syntax-parse s
         #:datum-literals (op)
         [(op _) #t]
         [_ #f])))

(define-name-root Syntax
  #:fields
  (literal
   literal_group
   make
   make_group
   make_sequence
   unwrap
   unwrap_group
   unwrap_sequence
   strip
   relocate
   relocate_span
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

(define (relevant-source-syntax ctx-stx-in)
  (syntax-parse ctx-stx-in
    #:datum-literals (group block alts parens brackets braces quotes multi op)
    [((~and head (~or group block alts parens brackets braces quotes)) . _) #'head]
    [(multi (g t))
     #:when (syntax-property #'g 'from-pack)
     (relevant-source-syntax #'t)]
    [(multi (g . _)) #'g]
    [(op o) #'o]
    [_ ctx-stx-in]))

(define (do-make who v ctx-stx tail? group?)
  ;; assume that any syntax objects are well-formed, while list structure
  ;; needs to be validated
  (define (invalid)
    (raise-arguments-error* 'Syntax.make rhombus-realm
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
  (datum->syntax ctx-stx (if group? (group v) (loop v tail?))))

(define/arity (make v [ctx-stx #f])
  (do-make 'Syntax.make v ctx-stx #t #f))

(define/arity (make_group v [ctx-stx #f])
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
  (unless (list? v) (raise-argument-error* 'Syntax.make_sequence rhombus-realm "List" v))
  (pack-multi (for/list ([e (in-list v)])
                (do-make 'Syntax.make_sequence e ctx-stx #t #t))))

(define/arity (unwrap v)
  (cond
    [(not (syntax? v))
     (raise-argument-error* 'Syntax.unwrap rhombus-realm "Syntax" v)]
    [else
     (define unpacked (unpack-term v 'Syntax.unwrap #f))
     (define u (syntax-e unpacked))
     (if (and (pair? u)
              (not (list? u)))
         (syntax->list unpacked)
         u)]))

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

(define/arity (strip v)
  (cond
    [(not (syntax? v))
     (raise-argument-error* 'Syntax.strip rhombus-realm "Syntax" v)]
    [else
     (strip-context v)]))

(define/arity (relocate stx ctx-stx-in)
  (unless (syntax? stx) (raise-argument-error* 'Syntax.relocate rhombus-realm "Syntax" stx))
  (unless (syntax? ctx-stx-in) (raise-argument-error* 'Syntax.relocate rhombus-realm "Syntax" ctx-stx-in))
  (define ctx-stx (relevant-source-syntax ctx-stx-in))
  #;(log-error "?? ~s" (syntax->datum stx))
  #;(log-error " : ~s" (syntax->datum ctx-stx-in))
  #;(log-error " = ~s" (syntax->datum ctx-stx))
  (define (relocate stx)
    #;(log-error " ! ~s" (syntax->datum stx))
    (datum->syntax stx (syntax-e stx) ctx-stx ctx-stx))
  (let loop ([stx stx])
    (syntax-parse stx
      #:datum-literals (group block alts parens brackets braces quotes multi op)
      [((~and head (~or group block alts parens brackets braces quotes)) . rest)
       (datum->syntax #f (cons (relocate #'head) #'rest))]
      [((~and m multi) (g t))
       #:when (syntax-property #'g 'from-pack)
       (loop #'t)]
      [((~and m multi) (g . rest))
       (datum->syntax #f (list #'m (cons (relocate #'g) #'rest)))]
      [((~and tag op) o)
       (datum->syntax #f (list #'tag (relocate #'o)))]
      [_
       (relocate stx)])))

(define relocate_method
  (lambda (stx)
    (let ([relocate (lambda (ctx-stx)
                      (relocate stx ctx-stx))])
      relocate)))

(define/arity (relocate_span stx ctx-stxes-in)
  (define keep-raw-interior? #f) ; expose this as an option?
  (unless (syntax? stx) (raise-argument-error* 'Syntax.relocate_span rhombus-realm "Syntax" stx))
  (define ctx-stxes (map relevant-source-syntax ctx-stxes-in))
  (define (combine-raw a b) (if (null? a) b (if (null? b) a (cons a b))))
  (let loop ([ctx-stxes (cdr ctx-stxes)]
             [loc (syntax-srcloc (car ctx-stxes))]
             [pre (or (syntax-raw-prefix-property (car ctx-stxes)) null)]
             [raw (if keep-raw-interior?
                      (or (syntax-raw-property (car ctx-stxes)) null)
                      null)]
             [suffix (combine-raw
                      (if keep-raw-interior?
                          (or (syntax-raw-tail-property (car ctx-stxes)) null)
                          null)
                      (if (or keep-raw-interior?
                              (null? (cdr ctx-stxes)))
                          (or (syntax-raw-suffix-property (car ctx-stxes)) null)
                          null))])
    (cond
      [(null? ctx-stxes)
       (let* ([ctx (datum->syntax #f #f loc)]
              [ctx (if (null? pre)
                       ctx
                       (syntax-raw-prefix-property ctx pre))]
              [ctx (syntax-raw-property ctx raw)]
              [ctx (if (null? suffix)
                       ctx
                       (syntax-raw-suffix-property ctx suffix))])
         (relocate stx ctx))]
      [(and (pair? (cdr ctx-stxes))
            (not keep-raw-interior?))
       (loop (cdr ctx-stxes) loc pre raw suffix)]
      [else
       (define empty-raw? (and (null? raw) (null? suffix)))
       (define ctx (car ctx-stxes))
       (define new-raw (or (syntax-raw-property ctx) null))
       (define new-loc (syntax-srcloc ctx))
       (loop (cdr ctx-stxes)
             (if (and loc
                      new-loc
                      (equal? (srcloc-source loc)
                              (srcloc-source new-loc)))
                 (srcloc (srcloc-source loc)
                         (srcloc-line loc)
                         (srcloc-column loc)
                         (srcloc-position loc)
                         (if (and (srcloc-position new-loc)
                                  (srcloc-span new-loc)
                                  (srcloc-position loc))
                             (- (+ (srcloc-position new-loc)
                                   (srcloc-span new-loc))
                                (srcloc-position loc))
                             (srcloc-span loc)))
                 loc)
             (if empty-raw?
                 (combine-raw pre (or (syntax-raw-prefix-property ctx) null))
                 pre)
             (if empty-raw?
                 (or (syntax-raw-property ctx) null)
                 (combine-raw (combine-raw (combine-raw raw suffix)
                                           (or (syntax-raw-prefix-property ctx) null))
                              (or (syntax-raw-property ctx) null)))
             (combine-raw (if keep-raw-interior?
                              (or (syntax-raw-tail-property ctx) null)
                              null)
                          (combine-raw
                           (or (syntax-raw-tail-suffix-property ctx) null)
                           (or (syntax-raw-suffix-property ctx) null))))])))

(define relocate_span_method
  (lambda (stx)
    (let ([relocate_span (lambda (ctx-stxes)
                           (relocate_span stx ctx-stxes))])
      relocate_span)))

(define syntax-method-table
  (hash 'unwrap (method1 unwrap)
        'unwrap_group (method1 unwrap_group)
        'unwrap_sequence (method1 unwrap_sequence)
        'strip (method1 strip)
        'relocate relocate_method
        'relocate_span relocate_span_method
        'srcloc (method1 syntax-srcloc)))

(define-syntax syntax-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(unwrap) (0ary #'unwrap)]
        [(unwrap_group) (0ary #'unwrap_group)]
        [(unwrap_sequence) (0ary #'unwrap_sequence)]
        [(strip) (0ary #'strip)]
        [(relocate) (nary #'relocate_method 1 #'relocate)]
        [(relocate_span) (nary #'relocate_span_method 1 #'relocate_span)]
        [(srcloc) (0ary #'get-srcloc)]
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

;; Needs "maybe":
#;
(define-static-info-syntax srcloc
  (#%call-result #,srcloc-static-infos))
