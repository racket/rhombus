#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     enforest/transformer
                     shrubbery/print
                     racket/phase+space
                     "realm.rkt"
                     "pack.rkt"
                     "name-path-op.rkt"
                     "dotted-sequence.rkt"
                     "realm.rkt"
                     "define-arity.rkt"
                     "call-result-key.rkt"
                     "name-root.rkt"
                     (submod "annotation.rkt" for-class)
                     (for-syntax racket/base)
                     (submod "syntax-object.rkt" for-quasiquote)
                     "srcloc.rkt")
         "space.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt")

(module+ for-unquote
  (provide (for-syntax syntax_meta.equal_binding)))

(begin-for-syntax
  (provide (for-space rhombus/namespace
                      syntax_meta)
           (for-space rhombus/annot
                      SyntaxPhase)
           ;; temporary backward compatibility:
           (rename-out [syntax_meta.error Syntax.error]))

  (define-name-root syntax_meta
    #:fields
    ([equal_binding syntax_meta.equal_binding]
     [equal_name_and_scopes syntax_meta.equal_name_and_scopes]
     expanding_phase
     [error syntax_meta.error]
     [value syntax_meta.value]
     [flip_introduce syntax_meta.flip_introduce]))

  (define expr-space-path (space-syntax #f))

  (define/arity (syntax_meta.value id/op
                                   [sp expr-space-path]
                                   [fail (lambda ()
                                           (raise-syntax-error 'syntax_meta.value "no binding" id/op))])
    (define id (extract-name 'syntax_meta.value id/op sp))
    (syntax-local-value id (if (and (procedure? fail)
                                    (procedure-arity-includes? fail 0))
                               fail
                               (lambda () fail))))

  (define (extract-free-name stx sp)
    (extract-name 'syntax_meta.equal_binding stx sp #:build-dotted? #t))

  (define/arity syntax_meta.equal_binding
    (case-lambda
      [(id1 id2) (free-identifier=? (extract-free-name id1 expr-space-path) (extract-free-name id2 expr-space-path))]
      [(id1 id2 sp) (free-identifier=? (extract-free-name id1 sp) (extract-free-name id2 sp))]
      [(id1 id2 sp phase1) (free-identifier=? (extract-free-name id1 sp) (extract-free-name id2 sp) phase1)]
      [(id1 id2 sp phase1 phase2) (free-identifier=? (extract-free-name id1 sp) (extract-free-name id2 sp) phase1 phase2)]))

  (define/arity syntax_meta.equal_name_and_scopes
    (case-lambda
      [(id1 id2) (syntax_meta.equal_name_and_scopes id1 id2 (syntax-local-phase-level))]
      [(id1 id2 phase)
       (define who 'syntax_meta.equal_name_and_scopes)
       (define l1 (extract-name-components who id1))
       (define l2 (extract-name-components who id2))
       (unless (phase? phase)
         (raise-argument-error* who rhombus-realm "SyntaxPhase" phase))
       (and (= (length l1) (length l2))
            (for/and ([n1 (in-list l1)]
                      [n2 (in-list l2)])
              (bound-identifier=? n1 n2 phase)))]))

  (define (extract-name who stx sp
                        #:lookup-dotted? [lookup-dotted? #t]
                        #:build-dotted? [build-dotted? #f])  
    (unless (space-name? sp) (raise-argument-error* who rhombus-realm "SpaceMeta" sp))
    (define in-space (let ([space-sym (space-name-symbol sp)])
                       (if space-sym
                           (make-interned-syntax-introducer space-sym)
                           (lambda (x) x))))
    (define s (unpack-term stx #f #f))
    (or (cond
          [(identifier? s) (in-space s)]
          [s
           (syntax-parse s
             #:datum-literals (op)
             [(op id) (in-space #'id)]
             [_ #f])]
          [else
           (define g (unpack-group stx #f #f))
           (and g
                (syntax-parse g
                  #:datum-literals (group)
                  [(group . (~var name (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref/maybe)))
                   (and (null? (syntax-e #'name.tail))
                        (in-space #'name.name))]
                  [(group n::dotted-operator-or-identifier-sequence)
                   (cond
                     [build-dotted?
                      (define l (syntax->list #'n))
                      (datum->syntax (car l)
                                     (build-dot-symbol l #:skip-dots? #t))]
                     [else
                      (datum->syntax #f 'none)])]
                  [_ #f]))])
        (raise-argument-error* who rhombus-realm "Name" stx)))

  (define (extract-name-components who stx)
    (define s (unpack-term stx #f #f))
    (or (cond
          [(identifier? s) (list s)]
          [s
           (syntax-parse s
             #:datum-literals (op)
             [(op id) (list #'id)]
             [_ #f])]
          [else
           (define g (unpack-group stx #f #f))
           (and g
                (syntax-parse g
                  #:datum-literals (group)
                  [(group n::dotted-operator-or-identifier-sequence)
                   (let loop ([l (syntax->list #'n)])
                     (cond
                       [(null? (cdr l)) l]
                       [else (cons (car l) (loop (cddr l)))]))]
                  [_ #f]))])
        (raise-argument-error* who rhombus-realm "Name" stx)))
  
  (define/arity (expanding_phase)
    (syntax-local-phase-level))

  (define/arity syntax_meta.error
    (case-lambda
      [(form) (raise-syntax-error (name-of form) "bad syntax" (maybe-respan form))]
      [(msg form) (raise-syntax-error (name-of form) msg (maybe-respan form))]
      [(msg form detail) (raise-syntax-error (name-of form) msg (maybe-respan form) (maybe-respan detail))]))

  (define (name-of stx)
    (syntax-parse stx
      #:datum-literals (group)
      [who:identifier (string->symbol (shrubbery-syntax->string #'who))]
      [(group who:identifier . _) (name-of #'who)]
      [(group . _) '?]
      [(multi (group who:identifier . _) . _) (name-of #'who)]
      [(multi . _) '?]
      [else #f]))

  (define/arity (syntax_meta.flip_introduce stx)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (transform-in stx))

  (define-annotation-syntax SyntaxPhase
    (identifier-annotation #'phase? #'())))
