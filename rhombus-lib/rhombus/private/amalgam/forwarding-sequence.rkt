#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "use-site.rkt"
                     "id-binding.rkt"
                     "import-check.rkt")
         "syntax-parameter.rkt"
         "static-info.rkt"
         "../version-case.rkt"
         "export-check.rkt")

;; The `rhombus-...-forwarding-sequence` forms handle definitions that are
;; only visible to later terms (as created with Rhombus `let`, say,
;; and exposed to here by a `rhombus-forward` wrapper). They also take
;; care of syntax parameters and making nested `import` work through
;; lifting. Forms like `rhombus-nested-forwarding-sequence` extend that
;; to gather `export` information for a nested context.

(provide rhombus-module-forwarding-sequence
         rhombus-block-forwarding-sequence
         rhombus-nested-forwarding-sequence
         rhombus-mixed-nested-forwarding-sequence

         ;; wrap `rhombus-forward` around a sequence of declarations
         ;; to make any bindings among the  declarations visible only
         ;; after the declarations
         rhombus-forward

         (for-syntax expand-forwarding-sequence
                     expand-forwarding-sequence-continue))

(define-syntax (rhombus-module-forwarding-sequence stx)
  (syntax-parse stx
    [(_ #:wrap-non-string proc . tail)
     ;; the "non-string" part is a shortcut for Scribble
     #`(sequence [(#:module proc) base-ctx add-ctx remove-ctx all-ctx #hasheq()] . tail)]
    [(_ . tail)
     #`(sequence [(#:module #f) base-ctx add-ctx remove-ctx all-ctx #hasheq()] . tail)]))

(define-syntax (rhombus-block-forwarding-sequence stx)
  (syntax-parse stx
    [(_ #:orig orig . tail)
     #`(sequence [(#:block #f orig) base-ctx add-ctx remove-ctx all-ctx #hasheq()] . tail)]))

(define-syntax (rhombus-nested-forwarding-sequence stx)
  ;; Used for something like `namespace`
  (syntax-parse stx
    [(_ final . tail)
     #`(sequence [(#:nested final) base-ctx add-ctx remove-ctx all-ctx #hasheq()] . tail)]))

(define-syntax (rhombus-mixed-nested-forwarding-sequence stx)
  ;; Used for something like `class`, where non-expression, non-definition
  ;; forms are expanded to `(quote-syntax (stop-id . _))` and gathered to
  ;; be passed along to `final`. Exports are also gathered.
  (syntax-parse stx
    [(_ (final . data) stop-id . tail)
     #`(sequence [(#:stop-at (final . data) stop-id ()) base-ctx add-ctx remove-ctx all-ctx #hasheq()] . tail)]))

(define-syntax (sequence stx)
  (forwarding-sequence-step stx syntax-local-context syntax-local-introduce))

;; A step for `sequence` takes a syntax object of the form
;;
;;    (_ sequence-state expr-or-defn ...)
;;
;; and produces one of the following:
;;
;;  * (sequence sequence-state expr-or-defn ...)
;;    to continue
;;
;;  * (begin
;;      expr-or-defn
;;      ...
;;      (sequence sequence-state expr-or-defn ...)
;;    to continue after the handled exprs and defns
;;
;;  * (final ....)
;;    to complete where `final` is provided when constructing
;;    an initial state
;;
;;  * (begin
;;      expr-or-defn
;;      ...
;;      (final ....))
;;
(define-for-syntax (forwarding-sequence-step stx get-expand-context local-introduce)
  (let loop ([stx stx] [accum null])
    (syntax-parse stx
      #:literals (quote)
      [(_ [state base-ctx add-ctx remove-ctx all-ctx stx-params])
       (define forms #`(begin #,@(reverse accum)))
       (syntax-parse #'state
         [(#:block-stop-at (final ...) _ accum ...)
          #`(begin
              #,forms
              (final ... #,@(reverse (syntax->list #'(accum ...)))))]
         [(#:stop-at (final ...) _ binds accum ...)
          #`(begin
              #,forms
              (final ... [#:ctx base-ctx remove-ctx] #,(reverse (syntax->list #'binds)) #,@(reverse (syntax->list #'(accum ...)))))]
         [(#:nested (final ...) bind ...)
          #`(begin
              #,forms
              (final ... [#:ctx base-ctx remove-ctx] #,@(reverse (syntax->list #'(bind ...)))))]
         [(#:block #f orig)
          (raise-syntax-error #f "block does not end with an expression" #'orig)]
         [(#:module _ prov ...)
          (unless (bound-identifier=? (datum->syntax #'base-ctx 'x)
                                      (datum->syntax #'remove-ctx 'x))
            (register-provide-check #'(base-ctx remove-ctx prov ...)))
          forms]
         [_ forms])]
      [(_ [state base-ctx add-ctx remove-ctx all-ctx stx-params] (~and form (quote v)) . forms)
       (loop #'(_ state base-ctx add-ctx remove-ctx all-ctx stx-params . forms)
             (cons (syntax-parse #'state
                     [(#:module #f . _) #'form]
                     [(#:module wrap . _ ) (if (string? (syntax-e #'v))
                                               #'form
                                               #'(wrap form))]
                     [_ #'form])
                   accum))]
      [(_ [state base-ctx add-ctx remove-ctx all-ctx stx-params] form . forms)
       (define exp-form (syntax-parse #'form
                          #:literals (module module*)
                          [(module . _) #'form]
                          [(module* . _) #'form]
                          [_
                           (with-continuation-mark
                            syntax-parameters-key #'stx-params
                            (local-expand #'form
                                          (get-expand-context)
                                          (list #'rhombus-forward
                                                #'pop-forward
                                                #'define-values
                                                #'define-syntaxes
                                                #'define-syntax-parameter
                                                ;; etc.
                                                #'begin
                                                #'provide
                                                #'#%require
                                                #'#%declare
                                                #'begin-for-syntax)
                                          #f))]))
       (define (need-end-expr state) (syntax-parse state
                                       [(#:block #t orig) #'(#:block #f orig)]
                                       [_ state]))
       (define (saw-end-expr state) (syntax-parse state
                                      [(#:block #f orig) #'(#:block #t orig)]
                                      [_ state]))
       (syntax-parse exp-form
         #:literals (begin define-values define-syntaxes rhombus-forward pop-forward #%require provide #%provide quote-syntax
                           define-syntax-parameter)
         [((~and tag rhombus-forward) sub-form ...)
          (define intro (let ([intro (syntax-local-make-definition-context-introducer (syntax-e #'tag))])
                          (lambda (stx)
                            (intro stx 'add))))
          #`(begin
              #,@(reverse accum)
              (sequence [state base-ctx #,(intro #'add-ctx) base-ctx #,(intro #'all-ctx) stx-params]
                        sub-form ...
                        (pop-forward add-ctx #,(intro #'remove-ctx)
                                     . #,(intro #'forms))))]
         [(pop-forward add-ctx remove-ctx . forms)
          #`(sequence [state base-ctx add-ctx remove-ctx all-ctx stx-params] . forms)]
         [(define-syntax-parameter key rhs)
          (with-syntax ([stx-params (syntax-parameter-update #'key #'rhs #'stx-params)]
                        [new-state (need-end-expr #'state)])
            #`(sequence [new-state base-ctx add-ctx remove-ctx all-ctx stx-params] . forms))]
         [(begin form-in ...)
          #:with (form ...) (map (lambda (form)
                                   (shift-origin form exp-form))
                                 (syntax->list #'(form-in ...)))
          (define seq #`(sequence [state base-ctx add-ctx remove-ctx all-ctx stx-params] form ... . forms))
          (if (null? accum)
              seq
              #`(begin #,@(reverse accum) #,seq))]
         [((~and def (~or* define-values define-syntaxes)) (id ...) rhs)
          #:do [(define sub (let ([sub (make-syntax-delta-introducer #'remove-ctx #'base-ctx)])
                              (lambda (stx) (sub stx 'remove))))
                (define add (let ([add (make-syntax-delta-introducer #'add-ctx #'base-ctx)])
                              (lambda (stx) (add stx 'add))))
                (define suball (let ([sub (make-syntax-delta-introducer #'all-ctx #'base-ctx)])
                                 (lambda (stx) (sub stx 'remove))))
                (define intro (lambda (stx)
                                (when (or
                                       ;; `def` after `let`:
                                       (and (identifier-binding stx)
                                            (identifier-distinct-binding stx (sub stx)))
                                       ;; `let` after `def`:
                                       (and (let ([ph (syntax-local-phase-level)]
                                                  [stx (syntax-local-identifier-as-binding
                                                        (local-introduce stx))])
                                              (identifier-binding (suball stx) ph #t #t))
                                            (not (bound-identifier=? stx (add stx)))))
                                  (raise-syntax-error #f "duplicate definition" stx))
                                (sub (add stx))))]
          #:with (new-id ...) (map intro (syntax->list #'(id ...)))
          #:with new-state (need-end-expr #'state)
          #`(begin
              #,@(reverse accum)
              #,(datum->syntax exp-form
                               (syntax-e #`(def (new-id ...)
                                             #,(if (eq? (syntax-e #'def) 'define-syntaxes)
                                                   (if (eqv? 0 (hash-count (syntax-e #'stx-params)))
                                                       #'rhs
                                                       #`(with-continuation-mark
                                                           syntax-parameters-key (quote-syntax stx-params)
                                                           rhs))
                                                   #`(with-syntax-parameters stx-params
                                                       #,(discard-static-infos #'rhs)))))
                               exp-form
                               exp-form)
              (sequence [new-state base-ctx add-ctx remove-ctx all-ctx stx-params] . forms))]
         [(#%require req ...)
          #:with new-state (need-end-expr #'state)
          (define sub (let ([sub (make-syntax-delta-introducer #'remove-ctx #'base-ctx)])
                        (lambda (stx) (sub stx 'remove))))
          (define add (let ([add (make-syntax-delta-introducer #'add-ctx #'base-ctx)])
                        (lambda (stx) (add stx 'add))))
          (define intro (lambda (stx) (sub (add stx))))
          (define check? (not (bound-identifier=? (datum->syntax #'base-ctx 'x)
                                                  (datum->syntax #'remove-ctx 'x))))
          (define reqs
            (for/list ([req (in-list (cdr (syntax->list exp-form)))])
              (when check?
                (check-require-bindings req sub))
              (syntax-parse req
                #:datum-literals (portal)
                [((~and tag portal) id content) #`(tag #,(intro #'id) content)]
                [_ (intro req)])))
          (syntax-parse #'new-state
            [((~or #:block #:block-stop-at) . _)
             (for ([req (in-list reqs)])
               ;; unlike normal `require`, `syntax-local-lift-require` doesn't remove
               ;; use-site scopes, so we have to do that ourselves here
               (syntax-local-lift-require (remove-use-site-scopes (local-introduce req)) #'use #f))
             #`(sequence [new-state base-ctx add-ctx remove-ctx all-ctx stx-params] . forms)]
            [_
             #`(begin
                 #,(syntax-track-origin #`(#%require #,@reqs) exp-form #'none)
                 (sequence [new-state base-ctx add-ctx remove-ctx all-ctx stx-params] . forms))])]
         [(provide prov ...)
          (define rev-prov (reverse (syntax->list #'(prov ...))))
          #`(begin
              #,@(reverse accum)
              #,@(syntax-parse #'state
                   [((~and tag #:module) head . tail)
                    (define new-state #`(tag head #,@rev-prov . tail))
                    (list
                     exp-form
                     #`(sequence [#,new-state base-ctx add-ctx remove-ctx all-ctx stx-params] . forms))]
                   [_
                    (define new-state
                      (syntax-parse #'state
                        [(#:stop-at head stop-id binds-tail . tail)
                         #`(#:stop-at head stop-id (#,@rev-prov . binds-tail) . tail)]
                        [[tag head . tail]
                         #`(tag head #,@rev-prov . tail)]))
                    (list #`(sequence [#,new-state base-ctx add-ctx remove-ctx all-ctx stx-params] . forms))]))]
         [(#%provide . _)
          (raise-syntax-error #f "shouldn't happen" exp-form)]
         [(quote-syntax (~and keep (id:identifier . _)) #:local)
          #:do [(define next
                  (syntax-parse #'state
                    [(#:block-stop-at head stop-id . tail)
                     (free-identifier=? #'id #'stop-id)
                     (syntax-track-origin
                      #`(begin
                          #,@(reverse accum)
                          (sequence [(#:block-stop-at head stop-id [keep stx-params] . tail)
                                     base-ctx add-ctx remove-ctx all-ctx stx-params] . forms))
                      exp-form
                      #'none)]
                    [(#:stop-at head stop-id binds . tail)
                     (free-identifier=? #'id #'stop-id)
                     (syntax-track-origin
                      #`(begin
                          #,@(reverse accum)
                          (sequence [(#:stop-at head stop-id binds [keep stx-params] . tail)
                                     base-ctx add-ctx remove-ctx all-ctx stx-params] . forms))
                      exp-form
                      #'none)]
                    [_ #f]))]
          #:when next
          next]
         [_ #`(begin
                #,@(reverse accum)
                #,(syntax-parse exp-form
                    #:literals (#%declare begin-for-syntax module module*)
                    [((~or* #%declare begin-for-syntax module module*) . _)
                     exp-form]
                    [_
                     (let ([exp-form
                            (cond
                              [(zero? (hash-count (syntax-e #'stx-params)))
                               exp-form]
                              [else #`(#%expression
                                       (with-syntax-parameters stx-params #,(discard-static-infos exp-form)))])])
                       (syntax-parse #'state
                         [(#:module #f . _) exp-form]
                         [(#:module wrap . _) #`(wrap #,exp-form)]
                         [_ exp-form]))])
                (sequence [#,(saw-end-expr #'state) base-ctx add-ctx remove-ctx all-ctx stx-params] . forms))])])))

;; TEMP approximate `syntax-local-make-definition-context-introducer`
(meta-if-version-at-least
 "8.12.0.8"
 (begin)
 (define-for-syntax (syntax-local-make-definition-context-introducer [name 'intdef])
   (define intdef-ctx (syntax-local-make-definition-context))
   (lambda (stx [mode 'flip])
     (internal-definition-context-introduce intdef-ctx stx mode))))

(define-syntax (rhombus-forward stx)
  (raise-syntax-error #f
                      "should not get expanded"
                      stx))

(define-syntax (pop-forward stx)
  (raise-syntax-error #f
                      "should not get expanded"
                      stx))

(define-for-syntax (expand-forwarding-sequence bodys accum-scopes stx-params local-introduce expr-k done-k)
  (expand-forwarding-sequence-continue
   #`[;; state:
      [(#:block-stop-at (expanded) expanded-accum) base-ctx add-ctx remove-ctx all-ctx #,stx-params]
      ;; bodys:
      (#,@bodys (quote-syntax (expanded-accum . #,accum-scopes) #:local))
      ;; expand-context:
      #,(list (gensym 'expand))]
   local-introduce
   ;; should lead to `expand-forwarding-sequence-continue`:
   expr-k
   ;; back to regularly scheduled expansion:
   done-k))

(define-for-syntax (expand-forwarding-sequence-continue all-state local-introduce expr-k done-k)
  (syntax-parse all-state
    [(state bodys expand-context)
     (define step-stx (forwarding-sequence-step #`(sequence state . bodys)
                                                (lambda () (syntax->datum #'expand-context))
                                                local-introduce))
     (syntax-parse step-stx
       #:literals (begin expanded)
       [(expanded [(-expanded-accum . accum-scopes) stx-params])
        (done-k null #'accum-scopes #'stx-params)]
       [(begin s-body ... (expanded [(-expanded-accum . accum-scopes) stx-params]))
        (done-k (syntax->list #'(s-body ...)) #'accum-scopes #'stx-params)]
       [(begin s-body ... (_ state . bodys))
        ;; should lead to `expand-forwarding-sequence-continue`:
        (expr-k (syntax->list #'(s-body ...))
                #`[state bodys expand-context])]
       [(_ state . bodys)
        (expand-forwarding-sequence-continue #`[state bodys expand-context] local-introduce expr-k done-k)])]))

;; sentinels for `expand-forwarding-sequence`
(define-syntax expanded #f)
(define-syntax expanded-accum #f)
