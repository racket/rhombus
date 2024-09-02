#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/strip-context
                     "module-path-parse.rkt")
         (submod "module-path.rkt" for-import-export)
         "declaration.rkt"
         "parens.rkt")

(provide (for-space rhombus/decl
                    (rename-out
                     [rhombus:module module]))
         pragma)

(module+ for-module-begin
  (provide (for-space rhombus/decl
                      rhombus:module)))

(module+ for-module+
  (provide rhombus-module+))

(define-decl-syntax rhombus:module
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (~optional #:splice)
           name:identifier
           (_::block body ...))
        (check-splicing stx)
        (list (datum->syntax
               #'name
               (syntax-e
                #`(rhombus-module+ name
                                   #:orig #,stx
                                   #:language #f
                                   body ...))))]
       [(_ #:splice
           name:identifier
           #:lang mp ...+
           (_::block body ...))
        #:with lang::module-path #'(group mp ...)
        (check-splicing stx)
        (list (datum->syntax
               #'name
               (syntax-e
                #`(rhombus-module+ name
                                   #:orig #,stx
                                   #:language #,(module-path-convert-parsed #'lang.parsed)
                                   body ...))))]
       [(_ name:identifier #:lang mp ...+ (_::block body ...))
        #:with lang::module-path #'(group mp ...)
        #`((module name #,(module-path-convert-parsed #'lang.parsed)
             (#,(datum->syntax #'lang.parsed '#%module-begin)
              (multi body
                     ...))))]
       [(_ (~optional (~and order (~or* #:late #:early))
                      #:defaults ([order #'#:early]))
           name:identifier
           #:lang mp ...+ (_::block body ...))
        #:with lang::module-path #'(group mp ...)
        #:with module (if (eq? (syntax-e #'order) '#:late) #'module* #'module)
        (when (and (eq? 'top-level (syntax-local-context))
                   (eq? (syntax-e #'order) '#:late))
          (raise-syntax-error #f
                              "late submodules are not supported in an interactive context"
                              stx
                              #'order))
        #`((module name #,(module-path-convert-parsed #'lang.parsed)
             (#,(datum->syntax #'lang.parsed '#%module-begin)
              (multi body
                     ...))))]))))

(define-for-syntax (check-splicing stx)
  (when (eq? 'top-level (syntax-local-context))
    (raise-syntax-error #f
                        "splicing submodules are not supported in an interactive context"
                        stx)))

;; ----------------------------------------

;; It would be better if `module+` somehow supported the body-wrapping
;; step that we need for Rhombus. But we start with mostly a copy of
;; `module+`, and need new features for Rhombus, anyway.

(begin-for-syntax
  (struct mod (lang-stx rev-body origs)))

(define-syntaxes (rhombus-module+)
  (lambda (stx)
    (unless (eq? (syntax-local-context) 'module)
      (error "a `rhombus-module+` was somehow misplaced"))
    (syntax-case stx ()
      [(_ the-submodule
          #:orig orig-stx
          #:language the-lang
          e ...)
       (begin
         ;; This looks it up the first time and is allowed to create a
         ;; list and lift a module-end declaration if necessary:
         (let ([stxs-box (get-stxs-box stx #'orig-stx #'the-submodule #t #'the-lang)])
           (define m (unbox stxs-box))
           (set-box! stxs-box
                     (struct-copy mod m
                                  [rev-body (append (reverse (syntax->list (syntax-local-introduce #'(e ...))))
                                                    (mod-rev-body m))]
                                  [origs (cons (syntax-local-introduce stx)
                                               (mod-origs m))])))
         (syntax/loc stx (begin)))])))

(begin-for-syntax
  ;; The following table is newly instantiated for each module
  ;; expansion that uses `module+', so it is effectively
  ;; module-local:
  (define-values (submodule->stxs-box) (make-weak-hash))
  (define (get-stxs-box ctx-stx form-stx the-submodule-stx lift? lang-stx)
    (define bx (hash-ref submodule->stxs-box (syntax-e the-submodule-stx) #f))
    (when bx
      (define m (unbox bx))
      (when form-stx
        (unless (equal? (syntax->datum lang-stx)
                        (syntax->datum (mod-lang-stx m)))
          (raise-syntax-error #f
                              "submodule has declarations with different languages"
                              form-stx
                              the-submodule-stx))))
    (hash-ref! submodule->stxs-box (syntax-e the-submodule-stx)
               (lambda ()
                 (when lift?
                   (syntax-local-lift-module-end-declaration
                    ;; Use the lexical context of the first `module+'
                    ;; form as the context of the implicit `#%module-begin':
                    (datum->syntax
                     ctx-stx
                     (list #'define-module the-submodule-stx lang-stx)
                     ctx-stx)))
                 (box (mod lang-stx null null))))))

;; A use of this form is lifted to the end of the enclosing module
;; for each submodule created by `module+':
(define-syntaxes (define-module)
  (lambda (stx)
    (syntax-case stx ()
      [(_ the-submodule the-lang)
       (let* ([stxs-box (get-stxs-box #f #f #'the-submodule #f #f)]
              ;; Propagate the lexical context of the first `module+'
              ;; for the implicit `#%module-begin':
              [module-decl
               (let ([maybe-strip-context
                      (lambda (v)
                        (if (syntax-e #'the-lang)
                            (strip-context (datum->syntax #f v))
                            v))])
                 (datum->syntax
                  stx
                  (list
                   #'module*
                   #'the-submodule
                   (and (syntax-e #'the-lang)
                        (maybe-strip-context #'the-lang))
                   (maybe-strip-context
                    (list
                     '#%module-begin
                     (cons
                      'multi
                      (map syntax-local-introduce (reverse (mod-rev-body (unbox stxs-box))))))))
                  stx))])
         ;; Add 'origin and copy properties for every original declaration
         (let loop ([stx module-decl]
                    [origs (mod-origs (unbox stxs-box))])
           (if (null? origs) stx
               (let* ([orig (car origs)]
                      [id-stx (if (symbol? (syntax-e orig)) orig
                                  (car (syntax-e orig)))])
                 (loop (syntax-track-origin stx orig id-stx)
                       (cdr origs))))))])))

;; ----------------------------------------

(begin-for-syntax
  (define-splicing-syntax-class :pragma
    #:attributes (kw [parsed 1])
    (pattern (~seq (~and kw #:unsafe))
             #:with (parsed ...) (list #'kw))
    (pattern (~seq (~and kw #:empty_evaluator))
             #:with (parsed ...) (list #'#:empty-namespace))
    ;; TODO: allow, but then disable `#:realm` added by `rhombus`:
    #;
    (pattern (~seq (~and kw #:realm) id:identifier)
             #:with (parsed ...) (list #'kw #'id))
    ;; not possible, so far:
    #;
    (pattern (~seq (~and kw #:cross_phase_persistent))
             #:with (parsed ...) (list #'#:cross-phase-persistent)))

  ;; Also module-local:
  (define-values (pragmas-box) (make-hasheq)))

(define-syntax pragma
  (declaration-transformer
   (lambda (stx)
     (define (check kw-stxes)
       (for ([kw (in-list (syntax->list kw-stxes))])
         (when (hash-ref pragmas-box (syntax-e kw) #f)
           (raise-syntax-error #f "duplicate use of keyword" stx kw))
         (hash-set! pragmas-box (syntax-e kw) #t)))
     (unless (eq? 'module (syntax-local-context))
       (raise-syntax-error #f "allowed only within a module body" stx))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ p::pragma)
        (check #'(p.kw))
        #`((#%declare p.parsed ...))]
       [(_ (_::block (group p::pragma ...)
                     ...))
        (check #'(p.kw ... ...))
        #`((#%declare p.parsed ... ... ...))]))))
