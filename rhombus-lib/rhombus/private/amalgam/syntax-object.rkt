#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "pack.rkt"
                     "srcloc.rkt")
         racket/symbol
         racket/treelist
         syntax/parse/pre
         syntax/strip-context
         shrubbery/property
         shrubbery/print
         "to-list.rkt"
         "injected.rkt"
         "provide.rkt"
         "expression.rkt"
         "normalize-syntax.rkt"
         (submod "annotation.rkt" for-class)
         "pack.rkt"
         "realm.rkt"
         "tag.rkt"
         "dotted-sequence.rkt"
         "name-equal.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "srcloc.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "index-result-key.rkt"
         "maybe-key.rkt"
         (submod "srcloc-object.rkt" for-static-info)
         (submod "string.rkt" static-infos)
         (submod "list.rkt" for-compound-repetition)
         "parens.rkt"
         "syntax-wrap.rkt"
         "context-stx.rkt"
         "static-info.rkt")

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
  (provide syntax-method-table
           syntax-field-table))

(module+ for-quasiquote
  (provide (for-syntax get-syntax-static-infos
                       get-treelist-of-syntax-static-infos
                       get-syntax-instances
                       set-parse-syntax-of-annotation!)))

;; Shadow normal `syntax?`
(define syntax? syntax*?)

(define-primitive-class Syntax syntax
  #:lift-declaration
  #:no-constructor-static-info
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  (literal
   literal_term
   literal_group
   literal_local
   literal_local_term
   literal_local_group
   [inject Syntax.inject]
   [make Syntax.make]
   [make_op Syntax.make_op]
   [make_group Syntax.make_group]
   [make_sequence Syntax.make_sequence]
   [make_id Syntax.make_id]
   [make_temp_id Syntax.make_temp_id]
   [relocate_split Syntax.relocate_split]
   [matched_of Syntax.matched_of])
  #:properties
  ()
  #:methods
  (unwrap
   unwrap_op
   unwrap_group
   unwrap_sequence
   unwrap_all
   strip_scopes
   replace_scopes
   name_to_symbol
   srcloc
   is_original
   relocate
   relocate_group
   relocate_span
   relocate_group_span
   relocate_ephemeral_span
   property
   group_property
   source_properties
   group_source_properties
   to_source_string))

(define-static-info-getter get-treelist-of-syntax-static-infos
  (#%index-result #,(get-syntax-static-infos))
  . #,(get-treelist-static-infos))

(define-annotation-syntax Identifier
  (identifier-annotation is-identifier? #,(get-syntax-static-infos)))
(define (is-identifier? s)
  (and (syntax*? s)
       (identifier? (unpack-term s #f #f))))

(define-annotation-syntax Operator
  (identifier-annotation is-operator? #,(get-syntax-static-infos)))
(define (is-operator? s)
  (and (syntax*? s)
       (let ([t (unpack-term s #f #f)])
         (syntax-parse t
           #:datum-literals (op)
           [(op _) #t]
           [_ #f]))))

(define-annotation-syntax Name
  (identifier-annotation syntax-name? #,(get-syntax-static-infos)))
(define (syntax-name? s)
  (and (syntax*? s)
       (let ([t (unpack-term s #f #f)])
         (or (identifier? t)
             (if t
                 (syntax-parse t
                   #:datum-literals (op)
                   [(op _) #t]
                   [_ #f])
                 (let ([t (unpack-group s #f #f #t)])
                   (and t
                        (syntax-parse t
                          #:datum-literals (group)
                          [(group _::dotted-operator-or-identifier-sequence) #t]
                          [_ #f]))))))))

(define-annotation-syntax IdentifierName
  (identifier-annotation syntax-identifier-name? #,(get-syntax-static-infos)))
(define (syntax-identifier-name? s)
  (and (syntax*? s)
       (let ([t (unpack-term s #f #f)])
         (or (identifier? t)
             (and (not t)
                  (let ([t (unpack-group s #f #f #t)])
                    (and t
                         (syntax-parse t
                           #:datum-literals (group)
                           [(group _::dotted-identifier-sequence) #t]
                           [_ #f]))))))))

(define-annotation-syntax Term
  (identifier-annotation syntax-term? #,(get-syntax-static-infos)))
(define (syntax-term? s)
  (and (syntax*? s)
       (unpack-term s #f #f)
       #t))

(define-annotation-syntax Group
  (identifier-annotation syntax-group? #,(get-syntax-static-infos)))
(define (syntax-group? s)
  (and (syntax*? s)
       (unpack-group s #f #f #t)
       #t))

(define-annotation-syntax TermSequence
  (identifier-annotation syntax-sequence? #,(get-syntax-static-infos)))
(define (syntax-sequence? s)
  (and (syntax*? s)
       (unpack-group-or-empty s #f #f)
       #t))

(define-annotation-syntax Block
  (identifier-annotation syntax-block? #,(get-syntax-static-infos)))
(define (syntax-block? s)
  (and (syntax*? s)
       (syntax-parse (unpack-term s #f #f)
         #:datum-literals (block)
         [(block . _) #t]
         [_ #f])
       #t))

(define-annotation-syntax Syntax.matched_of
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx ctx)
     (parse-syntax-of-annotation stx))))

(begin-for-syntax
  (define parse-syntax-of-annotation #f)
  (define (set-parse-syntax-of-annotation! proc) (set! parse-syntax-of-annotation proc)))

(define-for-syntax (add-span-and-syntax-static-info orig-stx e)
  (syntax-parse orig-stx
    [(form-id arg . _)
     (wrap-static-info*
      (relocate+reraw
       (respan (datum->syntax #f (list #'form-id #'arg)))
       e
       #:prop-stx e)
      (get-syntax-static-infos))]))

(define-syntax literal
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (~and ((~or* _::parens _::quotes) . _) gs) . tail)
        (values (add-span-and-syntax-static-info stx #`(quote-syntax #,(pack-tagged-multi #'gs))) #'tail)]))))

(define-syntax literal_term
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ ((~or* _::parens _::quotes) (group term)) . tail)
        (values (add-span-and-syntax-static-info stx #'(quote-syntax term)) #'tail)]))))

(define-syntax literal_group
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ ((~or* _::parens _::quotes) g) . tail)
        (values (add-span-and-syntax-static-info stx #'(quote-syntax g)) #'tail)]))))

(define-syntax literal_local
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (~and ((~or* _::parens _::quotes) . _) gs) . tail)
        (values (add-span-and-syntax-static-info stx #`(quote-syntax #,(pack-tagged-multi #'gs) #:local)) #'tail)]))))

(define-syntax literal_local_term
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ ((~or* _::parens _::quotes) (group term)) . tail)
        (values (add-span-and-syntax-static-info stx #'(quote-syntax term #:local)) #'tail)]))))

(define-syntax literal_local_group
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ ((~or* _::parens _::quotes) g) . tail)
        (values (add-span-and-syntax-static-info stx #'(quote-syntax g #:local)) #'tail)]))))

;; ----------------------------------------

(define (starts-alts? ds)
  (and (pair? ds)
       (let ([a (car ds)])
         (define e/l (if (syntax*? a)
                         (let ([t (unpack-term a #f #f)])
                           (and t (syntax-e t)))
                         a))
         (define e (or (and (not (pair? e/l))
                            (to-list #f e/l))
                       e/l))
         (cond
           [(pair? e)
            (define head-stx (car e))
            (define head (if (syntax*? head-stx) (syntax-e (syntax-unwrap head-stx)) head-stx))
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
                                "cannot coerce value to group syntax"
                                (if tail?
                                    "cannot coerce value to term syntax"
                                    "cannot coerce value to non-tail term syntax"))
                            "value" v))
  (define (group-loop l)
    (for/list ([e (in-list l)])
      (group e)))
  (define (group e)
    (cond
      [(and (pair? e)
            (list? e))
       (define head-stx (car e))
       (define head (if (syntax*? head-stx) (syntax-e (syntax-unwrap head-stx)) head-stx))
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
      [(syntax*? e)
       (or (unpack-group e #f #f #t)
           (invalid))]
      [else (invalid)]))
  (define (loop v pre-alt? tail?)
    (cond
      [(null? v) (invalid)]
      [(list? v)
       (define head-stx (car v))
       (define head (if (syntax*? head-stx) (syntax-e (syntax-unwrap head-stx)) head-stx))
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
                           (define head (if (syntax*? head-stx) (syntax-e (syntax-unwrap head-stx)) head-stx))
                           (if (eq? head 'block)
                               (loop e #f #t)
                               (invalid))]
                          [(to-list #f e) => tail-loop]
                          [(syntax*? e)
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
      [(syntax-wrap? v) (loop (syntax-unwrap v) pre-alt? tail?)]
      [(syntax*? v) (let ([t (unpack-term v #f #f)])
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
      [(syntaxable? v) v]
      [else (invalid)]))
  (datum->syntax ctx-stx-t (if group? (group v) (loop v pre-alts? tail?))))

(define/arity (Syntax.inject v [ctx-stx #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (cond
    [(syntax*? v) v]
    [else
     (define ctx-stx-t (extract-ctx who ctx-stx))
     (define new-v (cond
                     [(syntaxable? v)
                      v]
                     [(needs-injected? v)
                      (injected v)]
                     [else v]))
     (datum->syntax ctx-stx new-v)]))

(define/arity (Syntax.make v [ctx-stx #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (do-make who v ctx-stx #t #t #f))

(define (check-symbol who v)
  (unless (symbol? v)
    (raise-annotation-failure who v "Symbol")))

(define/arity (Syntax.make_op v [ctx-stx #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (check-symbol who v)
  (do-make who (list 'op v) ctx-stx #t #t #f))

(define (to-nonempty-list who v-in)
  (define l (to-list #f v-in))
  (unless (pair? l)
    (raise-annotation-failure who v-in "Listable.to_list && NonemptyList"))
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
    (raise-annotation-failure who s "ReadableString")))

(define/arity (Syntax.make_id str [ctx #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (check-readable-string who str)
  (define istr (string->immutable-string str))
  (syntax-raw-property (datum->syntax (extract-ctx who ctx)
                                      (string->symbol istr))
                       istr))

(define/arity (Syntax.make_temp_id [v #f] #:keep_name [keep-name? #f])
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (define id
    (cond
      [keep-name?
       (define sym
         (or (cond
               [(symbol? v) v]
               [(string? v) (string->symbol v)]
               [(syntax*? v)
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
  (unless (syntax*? v)
    (raise-annotation-failure who v "Syntax")))

(define (needs-injected? v)
  (or (hash? v)
      (pair? v)
      (null? v)
      (vector? v)
      (box? v)
      (prefab-struct-key v)))

(define/method (Syntax.unwrap v)
  (check-syntax who v)
  (define unpacked (unpack-term v who #f))
  (define u (syntax-e unpacked))
  (cond
    [(not (pair? u))
     (cond
       [(injected? u)
        (injected-e u)]
       [(needs-injected? u)
        ;; treat as opaque, but nomalized to term form
        unpacked]
       [else
        ;; assume that this is some injected value
        u])]
    [else
     (define head (syntax-e (car u)))
     (define (unwrap-list mode)
       (define l (syntax->list unpacked))
       (cond
         [(not l) unpacked]
         [else
          (cond
            [(for/and ([e (in-list (cdr l))])
               (define e-u (syntax-e e))
               (and (pair? e-u)
                    (eq? mode (syntax-e (car e-u)))))
             (list->treelist l)]
            [else unpacked])]))
     (case head
       [(parsed) v]
       [(parens brackets braces quotes)
        (unwrap-list 'group)]
       [(block)
        (unwrap-list 'group)]
       [(alts)
        (unwrap-list 'block)]
       [(op)
        (define l (syntax->list unpacked))
        (if (and l (pair? (cdr l)) (null? (cddr l)) (identifier? (cadr l)))
            (treelist (car l) (cadr l))
            unpacked)]
       [else unpacked])]))

(define/method (Syntax.unwrap_op v)
  (syntax-parse (and (syntax*? v)
                     (unpack-term v who #f))
    #:datum-literals (op)
    [(op o:identifier) (syntax-e #'o)]
    [_ (raise-annotation-failure who v "Operator")]))

(define/method (Syntax.unwrap_group v)
  #:static-infos ((#%call-result #,(get-treelist-of-syntax-static-infos)))
  (check-syntax who v)
  (define l (syntax->list (unpack-tail v who #f)))
  (if (pair? l)
      (list->treelist l)
      (raise-arguments-error* who rhombus-realm "invalid syntax for group" "syntax" v)))

(define/method (Syntax.unwrap_sequence v)
  #:static-infos ((#%call-result #,(get-treelist-of-syntax-static-infos)))
  (check-syntax who v)
  (define l (syntax->list (unpack-multi-tail v who #f)))
  (if l
      (list->treelist l)
      (raise-arguments-error* who rhombus-realm "invalid syntax for group sequence" "syntax" v)))

(define/method (Syntax.unwrap_all v)
  (check-syntax who v)
  (let loop ([v v] [mode 'any])
    (define u (syntax-e v))
    (cond
      [(not (pair? u))
       (cond
         [(not (or (eq? mode 'any)
                   (eq? mode 'term)))
          v]
         [(injected? u)
          (injected-e u)]
         [(needs-injected? u)
          ;; treat as opaque
          v]
         [else
          ;; assume that this is some injected value
          u])]
      [else
       (define head (syntax-e (car u)))
       (define (unwrap-list mode)
         (define l (syntax->list v))
         (cond
           [(not l) v]
           [else
            (define new-l
              (treelist-cons
               (for/treelist ([e (in-list (cdr l))])
                 (loop e mode))
               head))
            (cond
              [(and (not (eq? mode 'term))
                    (for/or ([e (in-treelist new-l)])
                      (syntax? e)))
               v]
              [else new-l])]))
       (case head
         [(parsed) v]
         [(multi)
          (define l
            (if (eq? mode 'any)
                (unwrap-list 'group)
                v))
          (if (and (treelist? l)
                   (= 2 (treelist-length l)))
              (let ([g (treelist-ref l 1)])
                (if (and (treelist? g)
                         (= 2 (treelist-length g)))
                    (treelist-ref g 1)
                    g))
              l)]
         [(group)
          (define l
            (if (or (eq? mode 'group)
                    (eq? mode 'any))
                (unwrap-list 'term)
                v))
          (if (and (eq? mode 'any)
                   (treelist? l)
                   (= 2 (treelist-length l)))
              (treelist-ref l 1)
              l)]
         [(op)
          (if (or (eq? mode 'term)
                  (eq? mode 'any))
              (let ([l (unwrap-list 'term)])
                (if (and (= 2 (treelist-length l))
                         (symbol? (treelist-ref l 1)))
                    l
                    v))
              v)]
         [(parens brackets braces quotes)
          (if (or (eq? mode 'term)
                  (eq? mode 'any))
              (unwrap-list 'group)
              v)]
         [(block)
          (if (or (eq? mode 'term)
                  (eq? mode 'block)
                  (eq? mode 'any))
              (unwrap-list 'group)
              v)]
         [(alts)
          (if (or (eq? mode 'term)
                  (eq? mode 'any))
              (unwrap-list 'block)
              v)]
         [else v])])))

(define/method (Syntax.strip_scopes v)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (check-syntax who v)
  (strip-context v))

(define/method (Syntax.replace_scopes v ctx)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (check-syntax who v)
  (replace-context (extract-ctx who ctx #:false-ok? #f) v))

(define/method (Syntax.name_to_symbol v)
  (name-to-symbol who v))

(define (do-relocate who stx-in ctx-stx-in
                     extract-ctx annot)
  (extract-ctx
   who stx-in
   #:update (lambda (stx container-mode)
              (define ctx-stx (cond
                                [(srcloc? ctx-stx-in)
                                 ctx-stx-in]
                                [(not ctx-stx-in) #f]
                                [else
                                 (extract-ctx
                                  who ctx-stx-in
                                  #:false-ok? #t
                                  #:annot annot)]))
              (cond
                [(syntax? ctx-stx)
                 (datum->syntax stx (syntax-e stx) ctx-stx ctx-stx)]
                [(srcloc? ctx-stx)
                 (if (eq? container-mode 'container)
                     (syntax-raw-srcloc-property stx ctx-stx)
                     (datum->syntax stx (syntax-e stx) ctx-stx stx))]
                [else
                 (let ([stx (syntax-raw-property (datum->syntax stx (syntax-e stx)) null)])
                   (if (eq? container-mode 'container)
                       (syntax-raw-srcloc-property
                        (syntax-raw-opaque-content-property stx null)
                        #f)
                       stx))]))
   #:update-tag
   (lambda (stx inner-stx)
     (datum->syntax #f (syntax-e stx) inner-stx stx))
   #:update-outer
   (lambda (stx inner-stx)
     (datum->syntax #f (syntax-e stx)))))

(define/method (Syntax.relocate stx ctx-stx)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (do-relocate who stx ctx-stx
               extract-ctx "maybe(Term || Srcloc)"))

(define/method (Syntax.relocate_group stx ctx-stx)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (do-relocate who stx ctx-stx
               extract-group-ctx "maybe(Group || Srcloc)"))

(define (make-do-syntax-property who extract-ctx)
  (case-lambda
    [(stx prop)
     (syntax-property (extract-ctx who stx #:false-ok? #f) prop)]
    [(stx prop val)
     (extract-ctx who stx
                  #:false-ok? #f
                  #:update (lambda (t container-mode)
                             (syntax-property t prop val)))]
    [(stx prop val preserved?)
     (extract-ctx who stx
                  #:false-ok? #f
                  #:update (lambda (t container-mode)
                             (syntax-property t prop val preserved?)))]))

(define/method Syntax.property
  #:static-infos ((#%call-result
                   (#:at_arities
                    ((4 ())
                     (24 #,(get-syntax-static-infos))))))
  (case-lambda
    [(stx prop)
     ((make-do-syntax-property who extract-ctx) stx prop)]
    [(stx prop val)
     ((make-do-syntax-property who extract-ctx) stx prop val)]
    [(stx prop val preserved?)
     ((make-do-syntax-property who extract-ctx) stx prop val (and preserved? #t))]))

(define/method Syntax.group_property
  #:static-infos ((#%call-result
                   (#:at_arities
                    ((4 ())
                     (24 #,(get-syntax-static-infos))))))
  (case-lambda
    [(stx prop)
     ((make-do-syntax-property who extract-group-ctx) stx prop)]
    [(stx prop val)
     ((make-do-syntax-property who extract-group-ctx) stx prop val)]
    [(stx prop val preserved?)
     ((make-do-syntax-property who extract-group-ctx) stx prop val (and preserved? #t))]))

(define/method Syntax.source_properties
  #:static-infos ((#%call-result
                   (#:at_arities
                    ((2 ())
                     (32 #,(get-syntax-static-infos))))))
  (case-lambda
    [(stx-in)
     (define stx (unpack-term/maybe stx-in))
     (unless stx (raise-annotation-failure who stx-in "Term"))
     (get-source-properties stx extract-ctx)]
    [(stx-in prefix raw tail suffix)
     (define stx (unpack-term/maybe stx-in))
     (unless stx (raise-annotation-failure who stx-in "Term"))
     (set-source-properties stx extract-ctx prefix raw tail suffix)]))

(define/method Syntax.group_source_properties
  #:static-infos ((#%call-result
                   (#:at_arities
                    ((2 ())
                     (32 #,(get-syntax-static-infos))))))
  (case-lambda
    [(stx-in)
     (define stx (unpack-group stx-in #f #f #t))
     (unless stx (raise-annotation-failure who stx-in "Group"))
     (get-source-properties stx extract-group-ctx)]
    [(stx-in prefix raw tail suffix)
     (define stx (unpack-group stx-in #f #f #t))
     (unless stx (raise-annotation-failure who stx-in "Group"))
     (set-source-properties stx extract-group-ctx prefix raw tail suffix)]))

(define (get-source-properties stx extract-ctx)
  (define-values (ctx container-mode) (extract-ctx 'get-source-properties stx #:report-container? #t))
  (if (eq? container-mode 'container)
      (values
       (or (combine-shrubbery-raw
            (syntax-raw-prefix-property ctx)
            (syntax-raw-inner-prefix-property ctx))
           null)
       (or (combine-shrubbery-raw
            (syntax-raw-property ctx)
            (syntax-raw-opaque-content-property ctx))
           null)
       (or (syntax-raw-tail-property ctx) null)
       (or (combine-shrubbery-raw
            (syntax-raw-inner-suffix-property ctx)
            (syntax-raw-suffix-property ctx))
           null))
      (values
       (or (combine-shrubbery-raw
            (syntax-raw-prefix-property ctx)
            (syntax-raw-inner-prefix-property ctx))
           null)
       (or (if (eq? container-mode 's-exp)
               (syntax-opaque-raw-property ctx)
               (syntax-raw-property ctx))
           null)
       null
       (or (combine-shrubbery-raw
            (syntax-raw-inner-suffix-property ctx)
            (syntax-raw-suffix-property ctx))
           null))))

(define (set-source-properties stx extract-ctx prefix raw tail suffix)
  (extract-ctx
   'get-source-properties stx
   #:update
   (lambda (stx container-mode)
     (let* ([stx (syntax-raw-prefix-property stx (if (null? prefix) #f prefix))]
            [stx (syntax-raw-inner-prefix-property stx #f)]
            [stx (syntax-raw-inner-suffix-property stx #f)]
            [stx (syntax-raw-suffix-property stx (if (null? suffix) #f suffix))]
            [stx (syntax-raw-tail-property stx (if (null? tail) #f tail))])
       (case container-mode
         [(container)
          (let* ([stx (syntax-raw-property stx '())])
            (syntax-raw-opaque-content-property stx (or raw null)))]
         [(s-exp)
          (syntax-opaque-raw-property stx (or raw null))]
         [else
          (syntax-raw-property stx (or raw null))])))))

;; also reraws, but in a mode that attaches raw test as permanent text,
;; instead of just ephmeral on the wrapper syntax object
(define/method (Syntax.relocate_span stx-in ctx-stxes-in)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (define stx (unpack-term/maybe stx-in))
  (unless stx (raise-annotation-failure who stx-in "Term"))
  (relocate-span who stx ctx-stxes-in extract-ctx))

(define/method (Syntax.relocate_group_span stx-in ctx-stxes-in)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (define stx (unpack-group stx-in #f #f #t))
  (unless stx (raise-annotation-failure who stx-in "Group"))
  (relocate-span who stx ctx-stxes-in extract-group-ctx))

(define/method (Syntax.relocate_ephemeral_span stx-in ctx-stxes-in)
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (unless (syntax*? stx-in) (raise-annotation-failure who stx-in "Syntax"))
  (relocate-span who stx-in ctx-stxes-in extract-ephemeral-ctx))

(define (to-list-of-stx who v-in)
  (define stxs (to-list #f v-in))
  (unless (and stxs (andmap syntax*? stxs))
    (raise-annotation-failure who v-in "Listable.to_list && List.of(Syntax)"))
  stxs)

(define (relocate-span who stx ctx-stxes-in extract-ctx)
  (define ctx-stxes (to-list-of-stx who ctx-stxes-in))
  (define new-stx
    (extract-ctx
     who stx
     #:update
     (lambda (stx container-mode)
       (define new-stx
         (if (null? ctx-stxes)
             ;; empty sequence: set everything to blank
             (let* ([stx (datum->syntax stx (syntax-e stx) #f stx)]
                    [stx (syntax-raw-property stx '())]
                    [stx (syntax-raw-prefix-property stx #f)]
                    [stx (syntax-raw-inner-prefix-property stx #f)]
                    [stx (syntax-raw-inner-suffix-property stx #f)]
                    [stx (syntax-raw-suffix-property stx #f)])
               (case container-mode
                 [(container)
                  (syntax-raw-opaque-content-property stx '())]
                 [(s-expr)
                  (syntax-opaque-raw-property stx '())]
                 [else stx]))
             (relocate+reraw (datum->syntax #f ctx-stxes) stx
                             #:keep-mode (case container-mode
                                           [(container) 'content]
                                           [(s-exp) #f]
                                           [else 'term]))))
       (syntax-relocated-property new-stx #t))))
  (syntax-relocated-property new-stx #t))

(define (to-list-of-term-stx who v-in)
  (define stxs (to-list #f v-in))
  (for/list ([stx (in-list (if (pair? stxs)
                               stxs
                               '(oops)))])
    (define term-stx (unpack-term/maybe stx))
    (unless term-stx
      (raise-annotation-failure who v-in "Listable.to_list && NonemptyList.of(Term)"))
    term-stx))

(define/arity (Syntax.relocate_split stxes-in ctx-stx)
  #:static-infos ((#%call-result #,(get-treelist-of-syntax-static-infos)))
  (define stxes (to-list-of-term-stx who stxes-in))
  (unless (syntax*? ctx-stx)
    (raise-annotation-failure who ctx-stx "Syntax"))
  (cond
    [(null? (cdr stxes))
     ;; copying from 1 to 1
     (treelist (Syntax.relocate (car stxes) ctx-stx))]
    [else
     (define ctx (Syntax.relocate_span (quote-syntax #f) (list ctx-stx)))
     (define loc (syntax-srcloc ctx))
     (define zero-width-loc (struct-copy srcloc loc [span 0]))
     (define (relocate-one stx
                           loc
                           #:prefix? [prefix? #f]
                           #:suffix? [suffix? #f]
                           raw)
       (extract-ctx
        who stx
        #:update
        (lambda (stx container-mode)
          (let* ([stx (datum->syntax stx
                                     (syntax-e stx)
                                     loc
                                     stx)]
                 [stx (syntax-raw-inner-prefix-property
                       stx
                       (and prefix?
                            (syntax-raw-inner-prefix-property ctx)))]
                 [stx (syntax-raw-prefix-property
                       stx
                       (and prefix?
                            (syntax-raw-prefix-property ctx)))])
            (cond
              [(eq? container-mode 'container)
               (let* ([stx (syntax-raw-property stx "")]
                      [stx (syntax-raw-opaque-content-property
                            stx
                            (if suffix?
                                (syntax-raw-property ctx)
                                ""))]
                      [stx (syntax-raw-tail-property stx #f)]
                      [stx (syntax-raw-suffix-property
                            stx
                            (and suffix?
                                 (syntax-raw-suffix-property ctx)))]
                      [stx (syntax-raw-inner-suffix-property
                            stx
                            (and suffix?
                                 (syntax-raw-inner-suffix-property ctx)))])
                 stx)]
              [else
               (let* ([stx ((if (eq? container-mode 's-exp) syntax-opaque-raw-property syntax-raw-property)
                            stx
                            (if suffix?
                                (syntax-raw-property ctx)
                                ""))]
                      [stx (syntax-raw-suffix-property
                            stx
                            (and suffix?
                                 (syntax-raw-suffix-property ctx)))]
                      [stx (syntax-raw-inner-suffix-property
                            stx
                            (and suffix?
                                 (syntax-raw-inner-suffix-property ctx)))])
                 stx)])))))
     (list->treelist
      (cons (relocate-one (car stxes)
                          zero-width-loc
                          #:prefix? #t
                          #:suffix? (null? (cdr stxes))
                          "")
            (let loop ([stxes (cdr stxes)])
              (cond
                [(null? (cdr stxes))
                 (list (relocate-one (car stxes)
                                     loc
                                     #:suffix? #t
                                     (or (syntax-opaque-raw-property ctx)
                                         (syntax-raw-property ctx))))]
                [else
                 (cons (relocate (car stxes)
                                 zero-width-loc
                                 null
                                 "")
                       (loop (cdr stxes)))]))))]))

(define/method (Syntax.to_source_string stx
                                        #:keep_prefix [keep-prefix? #f]
                                        #:keep_suffix [keep-suffix? #f]
                                        #:as_inner [inner? #t])
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (check-syntax who stx)
  (define norm-stx (normalize-syntax stx))
  (string->immutable-string (shrubbery-syntax->string (or (and (not inner?)
                                                               (unpack-term/maybe norm-stx))
                                                          norm-stx)
                                                      #:use-raw? #t
                                                      #:keep-prefix? keep-prefix?
                                                      #:keep-suffix? keep-suffix?
                                                      #:inner? inner?)))

(define/method (Syntax.srcloc stx)
  #:static-infos ((#%call-result ((#%maybe #,(get-srcloc-static-infos)))))
  (check-syntax who stx)
  (syntax-srcloc (maybe-respan stx)))

(define/method (Syntax.is_original v)
  (syntax-original? (extract-ctx who v #:false-ok? #f)))

(define (syntax-field-table s)
  (cond
    [(syntax-wrap? s)
     (define ht (syntax-wrap-attribs s))
     (for/hash ([(k v) (in-hash ht)])
       (values k (lambda (s)
                   (cond
                     [(eq? v 'ambiguous)
                      (raise-arguments-error*
                       k
                       rhombus-realm
                       "field name is ambiguous")]
                     [(eqv? 0 (cdr v))
                      (car v)]
                     [else
                      (raise-arguments-error*
                       k
                       rhombus-realm
                       "field is a repetition;\n use requires static mode")]))))]
    [else #f]))
