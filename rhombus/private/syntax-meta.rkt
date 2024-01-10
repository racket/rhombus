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
         "name-root-ref.rkt"
         "is-static.rkt")

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
     [expanding_phase syntax_meta.expanding_phase]
     [error syntax_meta.error]
     [value syntax_meta.value]
     [flip_introduce syntax_meta.flip_introduce]
     [is_static syntax_meta.is_static]))

  (define expr-space-path (space-syntax #f))

  (define/arity (syntax_meta.value id/op
                                   [sp expr-space-path]
                                   [fail (lambda ()
                                           (raise-syntax-error who "no binding" id/op))])
    (define id (extract-name who id/op sp))
    (syntax-local-value id (if (and (procedure? fail)
                                    (procedure-arity-includes? fail 0))
                               fail
                               (lambda () fail))))

  (define (extract-free-name who stx sp)
    (extract-name who stx sp #:build-dotted? #t))

  (define/arity syntax_meta.equal_binding
    (case-lambda
      [(id1 id2)
       (free-identifier=? (extract-free-name who id1 expr-space-path)
                          (extract-free-name who id2 expr-space-path))]
      [(id1 id2 sp)
       (free-identifier=? (extract-free-name who id1 sp)
                          (extract-free-name who id2 sp))]
      [(id1 id2 sp phase1)
       (free-identifier=? (extract-free-name who id1 sp)
                          (extract-free-name who id2 sp)
                          phase1)]
      [(id1 id2 sp phase1 phase2)
       (free-identifier=? (extract-free-name who id1 sp)
                          (extract-free-name who id2 sp)
                          phase1
                          phase2)]))

  (define/arity (syntax_meta.equal_name_and_scopes id1
                                                   id2
                                                   [phase (syntax-local-phase-level)])
    (define l1 (extract-name-components who id1))
    (define l2 (extract-name-components who id2))
    (unless (phase? phase)
      (raise-argument-error* who rhombus-realm "SyntaxPhase" phase))
    (and (= (length l1) (length l2))
         (for/and ([n1 (in-list l1)]
                   [n2 (in-list l2)])
           (bound-identifier=? n1 n2 phase))))

  (define (extract-name who stx sp
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
                   #:when (null? (syntax-e #'name.tail))
                   (in-space #'name.name)]
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

  (define/arity (syntax_meta.expanding_phase)
    (syntax-local-phase-level))

  (define/arity syntax_meta.error
    (case-lambda
      [(form) (raise-syntax-error (name-of form) "bad syntax" (maybe-respan form))]
      [(msg form) (raise-syntax-error (name-of form) msg (maybe-respan form))]
      [(msg form detail)
       (define details (map maybe-respan (if (list? detail) detail (list detail))))
       (if (pair? details)
           (raise-syntax-error (name-of form) msg
                               (maybe-respan form)
                               (car details)
                               (cdr details))
           (raise-syntax-error (name-of form) msg
                               (maybe-respan form)))]))

  (define (name-of stx)
    (syntax-parse stx
      #:datum-literals (multi group)
      [who:identifier (string->symbol (shrubbery-syntax->string #'who))]
      [(group who:identifier . _) (name-of #'who)]
      [(group . _) '?]
      [(multi (group who:identifier . _) . _) (name-of #'who)]
      [(multi . _) '?]
      [_ #f]))

  (define/arity (syntax_meta.flip_introduce stx)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (transform-in stx))

  (define (unpack-identifier-or-operator who id/op-in)
    (define id/op (unpack-term/maybe id/op-in))
    (define id
      (cond
        [(identifier? id/op) id/op]
        [id/op (syntax-parse id/op
                 #:datum-literals (op)
                 [(op o) #'o]
                 [_ #f])]
        [else #f]))
    (unless id
      (raise-argument-error* who rhombus-realm
                             "Identifier || Operator"
                             id/op-in))
    id)

  (define/arity (syntax_meta.is_static id/op-in)
    (define id (unpack-identifier-or-operator who id/op-in))
    (is-static-context? id))

  (define-annotation-syntax SyntaxPhase
    (identifier-annotation #'phase? #'())))
