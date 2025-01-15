#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/symbol
         syntax/parse/pre
         (only-in racket/base
                  [module-path? racket:module-path?])
         syntax/private/modcollapse-noctc
         "provide.rkt"
         "printer-property.rkt"
         "print-desc.rkt"
         (submod "module-path.rkt" for-import-export)
         "expression.rkt"
         "repetition.rkt"
         (submod "dot.rkt" for-dot-provider)
         "class-primitive.rkt"
         "define-arity.rkt"
         "static-info.rkt"
         "call-result-key.rkt"
         "maybe-key.rkt"
         "realm.rkt"
         "annotation-failure.rkt"
         "module-path-parse.rkt"
         "parens.rkt"
         "pack.rkt"
         "parens-sc.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace
                      rhombus/statinfo)
                     ModulePath)
         (for-spaces (#f
                      rhombus/repet)
                     (rename-out [ModulePath-form ModulePath])))

(module+ for-primitive
  (provide module-path
           module-path?
           module-path-s-exp
           module-path-resolved
           module-path-index-or-resolved
           module-path-s-exp-or-index-or-resolved))


(define/method (ModulePath.s_exp mp)
  (check-module-path who mp)
  (module-path-s-exp mp))

(define/method (ModulePath.add mp rel-mp)
  #:static-infos ((#%call-result ((#%maybe #,(get-module-path-static-infos)))))
  (check-module-path who mp)
  (check-module-path who rel-mp)
  (define mpi (module-path-index-join (module-path-s-exp rel-mp)
                                      (module-path-index-or-resolved mp)))
  (cond
    [(or (module-path-index? (module-path-raw rel-mp))
         (module-path-index? (module-path-raw mp)))
     (module-path mpi)]
    [else
     ;; stay in plain S-expression mode:
     (module-path (or (collapse-module-path-index mpi)
                      '(submod ".")))]))

(define-primitive-class ModulePath module-path
  #:no-constructor-static-info
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  ([try ModulePath.try])
  #:properties
  ()
  #:methods
  ([s_exp ModulePath.s_exp]
   [add ModulePath.add]))

(struct module-path (raw) ; `raw` can be an S-expression, resolved module path, or module path index
  #:authentic
  #:sealed
  #:property prop:field-name->accessor (list* '() module-path-method-table #hasheq())
  #:property prop:equal+hash (list
                              (lambda (v v2 eql?)
                                (eql? (module-path-raw v) (module-path-raw v2)))
                              (lambda (v hc)
                                (hc (module-path-raw v)))
                              (lambda (v hc)
                                (hc (module-path-raw v))))
  #:property prop:printer (lambda (v mode recur)
                            (pretty-listlike
                             "ModulePath("
                             (list (PrintDesc-doc
                                    (recur (datum->syntax
                                            #f
                                            (format-module-path
                                             (module-path-s-exp v))))))
                             ")")))

(define (format-module-path raw)
  (cond
    [(string? raw) `(group ,raw)]
    [(symbol? raw) `(group (op |.|) ,raw)]
    [else
     (case (car raw)
       [(file) `(group file (parens (group ,(cadr raw))))]
       [(lib) `(group lib (parens (group ,(cadr raw))))]
       [(quote) `(group (op |.|) ,(cadr raw))]
       [(submod)
        (define base (cadr raw))
        (define more (cddr raw))
        (define new-base
          (cond
            [(equal? base ".") '(group self)]
            [(equal? base "..") '(group parent)]
            [else (format-module-path base)]))
        (cond
          [(null? more) new-base]
          [else
           (let loop ([more more] [parents-extra '()])
             (cond
               [(and (pair? more) (equal? (car more) ".."))
                (loop (cdr more) (cons '(op !) parents-extra))]
               [else
                `(group ,@(cdr new-base)
                        ,@parents-extra
                        ,@(let loop ([more more])
                            (cond
                              [(null? more) null]
                              [(equal? (car more) "..")
                               (cdr more)]
                              [else
                               (list* '(op !)
                                      (car more)
                                      (loop (cdr more)))])))]))])]
       [else `(group (op ???) (group ,raw))])]))

(define-for-syntax (parse-module-path-form stx #:repet? [as-repet? #false])
  (syntax-parse stx
    #:datum-literals (group)
    [(_ (_::quotes mod-path::module-path) . tail)
     ;; syntactic form with static parsing based on binding
     (when as-repet?
       (raise-syntax-error #f
                           "static module path not supported in a repetition position"
                           stx))
     (values (wrap-static-info*
              #`(module-path (quote #,(convert-symbol-module-path #'mod-path.parsed)))
              (get-module-path-static-infos))
             #'tail)]
    [(_ . tail)
     ;; dynamic parsing based on syntax-object literals
     (values (if as-repet?
                 (identifier-repetition-use #'ModulePath)
                 #'ModulePath)
             #'tail)]))

(define-syntax ModulePath-form
  (expression-transformer
   (lambda (stx)
     (parse-module-path-form stx))))

(define-repetition-syntax ModulePath-form
  (repetition-transformer
   (lambda (stx)
     (parse-module-path-form stx #:repet? #t))))

(define (check-module-path who mp)
  (unless (module-path? mp)
    (raise-annotation-failure who mp "ModulePath")))

(define (module-path-s-exp mp)
  (define raw (module-path-raw mp))
  (cond
    [(resolved-module-path? raw)
     (let loop ([r (resolved-module-path-name raw)])
       (cond
         [(pair? r)
          `(submod ,(loop (car r)) ,@(cdr r))]
         [(path? r)
          r]
         [else `(quote ,r)]))]
    [(module-path-index? raw)
     (or (collapse-module-path-index raw)
         `(submod "."))]
    [else raw]))

(define (module-path-resolved mp)
  (define raw (module-path-raw mp))
  (cond
    [(resolved-module-path? raw)
     raw]
    [else (module-path-index-resolve raw)]))

(define (module-path-index-or-resolved mp)
  (define raw (module-path-raw mp))
  (cond
    [(or (resolved-module-path? raw)
         (module-path-index? raw))
     raw]
    [else (module-path-index-join raw #f)]))

(define (module-path-s-exp-or-index-or-resolved mp)
  (module-path-raw mp))

(define (parse-ModulePath who stx just-try?)
  (define g (and (syntax? stx) (unpack-group stx #f #f)))
  (unless g (raise-annotation-failure who stx "Group"))
  (define (bad)
    (and (not just-try?)
         (raise-arguments-error* who rhombus-realm "syntax object does not contain a valid module path"
                                 "syntax object" stx)))
  (define (check-and-wrap mp submods)
    (if (racket:module-path? mp)
        (module-path
         (if (null? (syntax-e submods))
             mp
             `(submod ,mp ,@(map syntax-e (syntax->list submods)))))
        (bad)))
  (syntax-parse g
    #:datum-literals (group lib file op self parent / ! |.|)
    [(group str:string (~seq (op !) sub:identifier) ...)
     (check-and-wrap (syntax-e #'str) #'(sub ...))]
    [(group lib (_::parens (group str:string)) (~seq (op !) sub:identifier) ...)
     (check-and-wrap `(lib ,(let ([s (module-lib-string-to-lib-string (syntax-e #'str))])
                              (and s (string->immutable-string s))))
                     #'(sub ...))]
    [(group file (_::parens (group str:string)) (~seq (op !) sub:identifier) ...)
     (check-and-wrap `(file ,(string->immutable-string (syntax-e #'str)))
                     #'(sub ...))]
    [(group self (~seq (op !) sub:identifier) ...+)
     (check-and-wrap "." #'(sub ...))]
    [(group parent (~and up (op !)) ... (~seq (op !) sub:identifier) ...)
     (check-and-wrap `(submod ".." ,@(map (lambda (up) "..") (syntax->list #'(up ...)))
                              ,@(map syntax-e (syntax->list #'(sub ...))))
                     #'())]
    [(group (op |.|) id:identifier (~seq (op !) sub:identifier) ...)
     (check-and-wrap `(quote ,(syntax-e #'id)) #'(sub ...))]
    [(group id:identifier (~seq (op !) sub:identifier) ...)
     #:when (not (memq (syntax-e #'id) '(file lib parent self)))
     (check-and-wrap `(lib ,(string->immutable-string
                             (module-symbol-to-lib-string (syntax-e #'id))))
                     #'(sub ...))]
    [(group id:identifier (~seq (op /) next-id:identifier) ... (~seq (op !) sub:identifier) ...)
     #:when (not (memq (syntax-e #'id) '(file lib parent self)))
     (check-and-wrap `(lib ,(string->immutable-string
                             (module-symbol-to-lib-string
                              (let loop ([nexts (syntax->list #'(next-id ...))]
                                         [accum (list (symbol->immutable-string (syntax-e #'id)))])
                                (cond
                                  [(null? nexts) (string->symbol (apply string-append (reverse accum)))]
                                  [else (loop (cdr nexts) (list* (symbol->immutable-string (syntax-e (car nexts)))
                                                                 "/"
                                                                 accum))])))))
                     #'(sub ...))]
    [_ (bad)]))

(define/arity (ModulePath stx)
  #:static-infos ((#%call-result #,(get-module-path-static-infos)))
  (parse-ModulePath who stx #f))

(define/arity (ModulePath.try stx)
  #:static-infos ((#%call-result ((#%maybe #,(get-module-path-static-infos)))))
  (parse-ModulePath who stx #t))
