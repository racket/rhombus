#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "pack.rkt")
         syntax/parse/pre
         syntax/strip-context
         racket/symbol
         shrubbery/property
         shrubbery/print
         "treelist.rkt"
         "to-list.rkt"
         "provide.rkt"
         "expression.rkt"
         (submod "annotation.rkt" for-class)
         "pack.rkt"
         "realm.rkt"
         "tag.rkt"
         "dotted-sequence.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "srcloc.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         (submod "srcloc-object.rkt" for-static-info)
         (submod "string.rkt" static-infos)
         (submod "list.rkt" for-compound-repetition)
         "parens.rkt"
         "context-stx.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Syntax)
         (for-spaces (rhombus/annot)
                     Term
                     Group
                     TermSequence
                     Block
                     Identifier
                     Operator
                     Name
                     IdentifierName))

(module+ for-builtin
  (provide syntax-method-table))

(module+ for-quasiquote
  (provide (for-syntax get-syntax-static-infos)))

(define-primitive-class Syntax syntax
  #:lift-declaration
  #:no-constructor-static-info
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  (literal
   literal_group
   [make Syntax.make]
   [make_op Syntax.make_op]
   [make_group Syntax.make_group]
   [make_sequence Syntax.make_sequence]
   [make_id Syntax.make_id]
   [make_temp_id Syntax.make_temp_id]
   )
  #:properties
  ()
  #:methods
  (unwrap
   unwrap_op
   unwrap_group
   unwrap_sequence
   unwrap_all
   srcloc
   is_original
   strip_scopes
   replace_scopes
   relocate
   relocate_span
   property
   to_source_string
   ))

(define-for-syntax (get-treelist-of-syntax-static-infos)
  #`((#%index-result #,(get-syntax-static-infos))
     . #,(get-treelist-static-infos)))

(define-annotation-syntax Syntax
  (identifier-annotation syntax? #,(get-syntax-static-infos)))

(define-annotation-syntax Identifier
  (identifier-annotation is-identifier? #,(get-syntax-static-infos)))
(define (is-identifier? s)
  (and (syntax? s)
       (identifier? (unpack-term s #f #f))))

(define-annotation-syntax Operator
  (identifier-annotation is-operator? #,(get-syntax-static-infos)))
(define (is-operator? s)
  (and (syntax? s)
       (let ([t (unpack-term s #f #f)])
         (syntax-parse t
           #:datum-literals (op)
           [(op _) #t]
           [_ #f]))))

(define-annotation-syntax Name
  (identifier-annotation syntax-name? #,(get-syntax-static-infos)))
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
  (identifier-annotation syntax-identifier-name? #,(get-syntax-static-infos)))
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
  (identifier-annotation syntax-term? #,(get-syntax-static-infos)))
(define (syntax-term? s)
  (and (syntax? s)
       (unpack-term s #f #f)
       #t))

(define-annotation-syntax Group
  (identifier-annotation syntax-group? #,(get-syntax-static-infos)))
(define (syntax-group? s)
  (and (syntax? s)
       (unpack-group s #f #f)
       #t))

(define-annotation-syntax TermSequence
  (identifier-annotation syntax-sequence? #,(get-syntax-static-infos)))
(define (syntax-sequence? s)
  (and (syntax? s)
       (unpack-group-or-empty s #f #f)
       #t))

(define-annotation-syntax Block
  (identifier-annotation syntax-block? #,(get-syntax-static-infos)))
(define (syntax-block? s)
  (and (syntax? s)
       (syntax-parse (unpack-term s #f #f)
         #:datum-literals (block)
         [(block . _) #t]
         [_ #false])
       #t))

(define-syntax literal
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ ((~or* _::parens _::quotes) (group term)) . tail)
        ;; Note: discarding group properties in this case
        (values #'(quote-syntax term) #'tail)]
       [(_ (~and ((~or* _::parens _::quotes) . _) gs) . tail)
        (values #`(quote-syntax #,(pack-tagged-multi #'gs)) #'tail)]))))

(define-syntax literal_group
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ ((~or* _::parens _::quotes)) . tail)
        (values #`(quote-syntax #,(pack-multi '())) #'tail)]
       [(_ ((~or* _::parens _::quotes) g) . tail)
        (values #'(quote-syntax g) #'tail)]))))

;; ----------------------------------------

(define (starts-alts? ds)
  (and (pair? ds)
       (let ([a (car ds)])
         (define e/l (if (syntax? a)
                         (let ([t (unpack-term a #f #f)])
                           (and t (syntax-e t)))
                         a))
         (define e (or (and (not (pair? e/l))
                            (to-list #f e/l))
                       e/l))
         (cond
           [(pair? e)
            (define head-stx (car e))
            (define head (if (syntax? head-stx) (syntax-e head-stx) head-stx))
            (case head
              [(alts) #t]
              [else #f])]
           [else #f]))))

(define (do-make who v ctx-stx pre-alts? tail? group?)
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
                      (cons (loop (car es)
                                  (starts-alts? ds)
                                  (null? ds))
                            (l-loop ds))])))
           (invalid))]
      [(to-list #f e) => group]
      [(syntax? e)
       (or (unpack-group e #f #f)
           (invalid))]
      [else (invalid)]))
  (define (loop v pre-alt? tail?)
    (cond
      [(null? v) (invalid)]
      [(list? v)
       (define head-stx (car v))
       (define head (if (syntax? head-stx) (syntax-e head-stx) head-stx))
       (case head
         [(parens brackets braces quotes)
          (cons head-stx (group-loop (cdr v)))]
         [(block)
          (if (or tail? pre-alt?)
              (cons head-stx (group-loop (cdr v)))
              (invalid))]
         [(alts)
          (if tail?
              (cons head-stx
                    (for/list ([e (in-list (cdr v))])
                      (let tail-loop ([e e])
                        (cond
                          [(and (pair? e)
                                (list? e))
                           (define head-stx (car e))
                           (define head (if (syntax? head-stx) (syntax-e head-stx) head-stx))
                           (if (eq? head 'block)
                               (loop e #f #t)
                               (invalid))]
                          [(to-list #f e) => tail-loop]
                          [(syntax? e)
                           (define u (unpack-term e #f #f))
                           (define d (and u (syntax-e u)))
                           (or (and d
                                    (eq? 'block (syntax-e (car d)))
                                    u)
                               (invalid))]
                          [else (invalid)]))))
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
      [(to-list #f v) => (lambda (v) (loop v pre-alt? tail?))]
      [(syntax? v) (let ([t (unpack-term v #f #f)])
                     (cond
                       [t
                        (define e (syntax-e t))
                        (cond
                          [(pair? e)
                           (define head-stx (car e))
                           (define head (syntax-e head-stx))
                           (case head
                             [(block)
                              (unless (or pre-alt? tail?) (invalid))]
                             [(alts)
                              (unless tail? (invalid))])])
                        t]
                       [else (invalid)]))]
      [else v]))
  (datum->syntax ctx-stx-t (if group? (group v) (loop v pre-alts? tail?))))

(define/arity (Syntax.make v [ctx-stx #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (do-make who v ctx-stx #t #t #f))

(define (check-symbol who v)
  (unless (symbol? v)
    (raise-argument-error* who rhombus-realm "Symbol" v)))

(define/arity (Syntax.make_op v [ctx-stx #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (check-symbol who v)
  (do-make who (list 'op v) ctx-stx #t #t #f))

(define (to-nonempty-list who v-in)
  (define l (to-list #f v-in))
  (unless (pair? l)
    (raise-argument-error* who rhombus-realm "Listable.to_list && NonemptyList" v-in))
  l)

(define/arity (Syntax.make_group v-in [ctx-stx #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (define v (to-nonempty-list who v-in))
  (define terms (let loop ([es v])
                  (cond
                    [(null? es) null]
                    [else
                     (define ds (cdr es))
                     (cons (do-make who (car es) ctx-stx
                                    (starts-alts? ds)
                                    (null? ds)
                                    #f)
                           (loop ds))])))
  (datum->syntax #f (cons group-tag terms)))

(define/arity (Syntax.make_sequence v [ctx-stx #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (pack-multi (for/list ([e (in-list (to-list who v))])
                (do-make who e ctx-stx #t #t #t))))

(define (check-readable-string who s)
  (unless (string? s)
    (raise-argument-error* who rhombus-realm "ReadableString" s)))

(define/arity (Syntax.make_id str [ctx #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (check-readable-string who str)
  (define istr (string->immutable-string str))
  (syntax-raw-property (datum->syntax (extract-ctx who ctx)
                                      (string->symbol istr))
                       istr))

(define/arity (Syntax.make_temp_id [v #false] #:keep_name [keep-name? #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (define id
    (cond
      [keep-name?
       (define sym
         (or (cond
               [(symbol? v) v]
               [(string? v) (string->symbol v)]
               [(syntax? v)
                (define sym (unpack-term v #f #f))
                (and (identifier? sym) (syntax-e sym))]
               [else #f])
             (raise-arguments-error* who rhombus-realm
                                     "name for ~keep_name is not an identifier, symbol, or string"
                                     "name" v)))
       ((make-syntax-introducer) (datum->syntax #f sym))]
      [else
       (car (generate-temporaries (list v)))]))
  (syntax-raw-property id (symbol->immutable-string (syntax-e id))))

(define (check-syntax who v)
  (unless (syntax? v)
    (raise-argument-error* who rhombus-realm "Syntax" v)))

(define/method (Syntax.unwrap v)
  (check-syntax who v)
  (define unpacked (unpack-term v who #f))
  (define u (syntax-e unpacked))
  (cond
    [(and (pair? u)
          (eq? (syntax-e (car u)) 'parsed))
     v]
    [else
     (if (and (pair? u)
              (not (list? u)))
         (list->treelist (syntax->list unpacked))
         (maybe-list->treelist u))]))

(define/method (Syntax.unwrap_op v)
  (check-syntax who v)
  (syntax-parse (unpack-term v who #f)
    #:datum-literals (op)
    [(op o) (syntax-e #'o)]
    [_
     (raise-arguments-error* who rhombus-realm
                             "syntax object does not have just an operator"
                             "syntax object" v)]))

(define/method (Syntax.unwrap_group v)
  #:static-infos ((#%call-result #,(get-treelist-of-syntax-static-infos)))
  (check-syntax who v)
  (list->treelist (syntax->list (unpack-tail v who #f))))

(define/method (Syntax.unwrap_sequence v)
  #:static-infos ((#%call-result #,(get-treelist-of-syntax-static-infos)))
  (check-syntax who v)
  (list->treelist (syntax->list (unpack-multi-tail v who #f))))

(define/method (Syntax.unwrap_all v)
  (check-syntax who v)
  (define (list->treelist* s)
    (cond
      [(null? s) empty-treelist]
      [(pair? s) (for/treelist ([e (in-list s)])
                   (list->treelist* e))]
      [else s]))
  (define (normalize s)
    (cond
      [(null? s) empty-treelist]
      [(not (pair? s)) s]
      [(eq? (car s) 'group)
       (if (null? (cddr s))
           (list->treelist* (cadr s))
           s)]
      [(eq? (car s) 'multi)
       (if (and (pair? (cdr s)) (null? (cddr s)))
           (normalize (cadr s))
           (list->treelist* s))]
      [else (list->treelist* s)]))
  (normalize (syntax->datum v)))

(define/method (Syntax.strip_scopes v)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (check-syntax who v)
  (strip-context v))

(define/method (Syntax.replace_scopes v ctx)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (check-syntax who v)
  (replace-context (extract-ctx who ctx #:false-ok? #f) v))

(define (check-valid-srcloc who ctx)
  (unless (or (syntax? ctx) (srcloc? ctx) (not ctx))
    (raise-argument-error* who rhombus-realm "Syntax || Srcloc || False" ctx)))

(define/method (Syntax.relocate stx ctx-stx)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (check-syntax who stx)
  (check-valid-srcloc who ctx-stx)
  (let ([ctx-stx (and (syntax? ctx-stx)
                      (relevant-source-syntax ctx-stx))])
    (at-relevant-dest-syntax
     stx
     (lambda (stx)
       (if (syntax? ctx-stx)
           (datum->syntax stx (syntax-e stx) ctx-stx ctx-stx)
           (datum->syntax stx (syntax-e stx) ctx-stx stx)))
     (lambda (stx inner-stx)
       stx))))

(define (relevant-source-syntax ctx-stx-in)
  (syntax-parse ctx-stx-in
    #:datum-literals (group block alts parens brackets braces quotes multi op)
    [((~and head (~or* group block alts parens brackets braces quotes)) . _) #'head]
    [(multi (g t))
     #:when (syntax-property #'g 'from-pack)
     (relevant-source-syntax #'t)]
    [(multi (g . _)) #'g]
    [(op o) #'o]
    [_ ctx-stx-in]))

;; Extracts from a suitable syntax object inside the source (e.g., a
;; `parens` tag or `group` tag) and similarly applies to a suitable
;; syntax object in the target.
(define (at-relevant-dest-syntax stx proc proc-outer)
  (let loop ([stx stx])
    (syntax-parse stx
      #:datum-literals (group block alts parens brackets braces quotes multi op)
      [((~and head (~or* group block alts parens brackets braces quotes)) . rest)
       (define inner (proc #'head))
       (proc-outer (datum->syntax #f (cons inner #'rest)) inner)]
      [((~and m multi) (g t))
       #:when (syntax-property #'g 'from-pack)
       (loop #'t)]
      [((~and m multi) (g . rest))
       (define inner (proc #'g))
       (proc-outer (datum->syntax #f (list #'m (cons inner #'rest))) inner)]
      [((~and tag op) o)
       (define inner (proc #'o))
       (proc-outer (datum->syntax #f (list #'tag inner)) inner)]
      [((~and tag parsed) space o)
       (define inner (proc #'o))
       (datum->syntax #f (list #'tag #'space inner) inner inner)]
      [_
       (proc stx)])))

(define (to-list-of-stx who v-in)
  (define stxs (to-list #f v-in))
  (unless (and stxs (andmap syntax? stxs))
    (raise-argument-error* who rhombus-realm
                           "Listable.to_list && List.of(Syntax)"
                           v-in))
  stxs)

;; also reraws:
(define/method (Syntax.relocate_span stx-in ctx-stxes-in)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (define stx (unpack-term/maybe stx-in))
  (unless stx (raise-argument-error* who rhombus-realm "Term" stx-in))
  (define ctx-stxes (to-list-of-stx who ctx-stxes-in))

  (at-relevant-dest-syntax
   stx
   (lambda (stx)
     (if (null? ctx-stxes)
         (let* ([stx (syntax-opaque-raw-property (syntax->datum stx (syntax-e stx) #f stx) '())]
                [stx (if (syntax-raw-prefix-property stx)
                         (syntax-raw-prefix-property stx '())
                         stx)]
                [stx (if (syntax-raw-suffix-property stx)
                         (syntax-raw-suffix-property stx '())
                         stx)])
           stx)
         (relocate+reraw (datum->syntax #f ctx-stxes) stx)))
   (lambda (stx inner-stx)
     (let ([stx (syntax-opaque-raw-property (datum->syntax #f (syntax-e stx) inner-stx)
                                            (syntax-opaque-raw-property inner-stx))])
       (let ([pfx (syntax-raw-prefix-property inner-stx)]
             [sfx (syntax-raw-suffix-property inner-stx)])
         (let* ([stx (if pfx
                         (syntax-raw-prefix-property stx pfx)
                         stx)]
                [stx (if sfx
                         (syntax-raw-suffix-property stx sfx)
                         stx)])
           stx))))))

(define/method (Syntax.to_source_string stx)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (check-syntax who stx)
  (string->immutable-string (shrubbery-syntax->string stx)))

(define/method (Syntax.srcloc stx)
  #:static-infos ((#%call-result #,(get-srcloc-static-infos)))
  (check-syntax who stx)
  (syntax-srcloc (maybe-respan stx)))

(define/method Syntax.property
  #:static-infos ((#%call-result
                   (#:at_arities
                    ((8 ())
                     (16 #,(get-syntax-static-infos))))))
  (case-lambda
    [(stx prop)
     (syntax-property (extract-ctx who stx #:false-ok? #f) prop)]
    [(stx prop val)
     (extract-ctx who stx
                  #:false-ok? #f
                  #:update (lambda (t)
                             (syntax-property t prop val)))]
    [(stx prop val preserved?)
     (extract-ctx who stx
                  #:false-ok? #f
                  #:update (lambda (t)
                             (syntax-property t prop val preserved?)))]))

(define/method (Syntax.is_original v)
  (syntax-original? (extract-ctx who v #:false-ok? #f)))
