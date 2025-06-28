#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "id-binding.rkt"
                     "introducer.rkt")
         "only-spaces-out.rkt"
         "all-spaces-out.rkt"
         "dotted-sequence-parse.rkt")

(provide maybe-provide-id
         maybe-provide-req)

(define-syntax maybe-provide-id
  (lambda (stx)
    (syntax-parse stx
      [(_ ex-id id ...)
       #`(provide
          #,@(for*/list ([id (in-list (syntax->list #'(id ...)))]
                         [space-sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                         #:do [(define intro
                                 (if space-sym
                                     (make-interned-syntax-introducer/add space-sym)
                                     (lambda (x) x)))
                               (define maybe-id (datum->syntax #'ex-id (syntax-e id)))
                               (define maybe-id-in-space (intro maybe-id))]
                         #:when (and (free-identifier=? maybe-id-in-space (intro id))
                                     (or (not space-sym)
                                         (identifier-distinct-binding* maybe-id-in-space maybe-id))))
               (define id/rename
                 (cond
                   [(identifier-extension-binding-tail-name maybe-id-in-space)
                    => (lambda (tail-name)
                         #`(#,maybe-id #,tail-name))]
                   [else maybe-id]))
               #`(only-spaces-out (all-spaces-out #,id/rename) #,space-sym)))])))

(define-syntax (maybe-provide-req stx)
  (syntax-parse stx
    [(_ ex-id req ...)
     ;; For the general case, we expand to
     ;;  (begin
     ;;    (require req)
     ;;    (provide (all-defined-out mp))]
     ;; for the module path `mp` in `req` and with a fresh scope to
     ;; distingiush eeach `req` from any other use of the same module
     ;; path. When possible, however, generate a provide of an `id`
     ;; directly, because that can work for reproviding in a namespace.
     ;; Also, portals have to be handled with a `maybe-provide-id`, since
     ;; they're more like definitions.
     (define (fail msg)
       (raise-syntax-error #f msg #'ex-id))
     (define (fail-phase)
       (fail "cannot export bindings imported at different phases"))
     (let seq-loop ([reqs-stx #'(req ...)] [phase-level 0] [only-phase #f]
                                           [just-space #f] [for-space #f])
       #`(begin
           #,@(for/list ([req (in-list (syntax->list reqs-stx))])
                (let loop ([req req] [phase-level phase-level] [only-phase only-phase]
                                     [just-space just-space] [for-space for-space])
                  (define (make-maybe-provide id)
                    (cond
                      [(or just-space for-space)
                       (define space-sym (syntax-e (or just-space for-space)))
                       (define intro
                         (if space-sym
                             (make-interned-syntax-introducer/add space-sym)
                             (lambda (x) x)))
                       (define maybe-id (datum->syntax #'ex-id (syntax-e id)))
                       (define maybe-id-in-space (intro maybe-id))
                       (cond
                         [(and (free-identifier=? maybe-id-in-space (intro id))
                               (or (not space-sym)
                                   (identifier-distinct-binding* maybe-id-in-space maybe-id)))
                          #`(provide (only-spaces-out (all-spaces-out #,maybe-id) #,space-sym))]
                         [else
                          #'(begin)])]
                      [else
                       #`(maybe-provide-id ex-id #,id)]))
                  (define (reprovide mp)
                    (cond
                      [(bound-identifier=? (datum->syntax #f 'x)
                                           ((make-syntax-delta-introducer req #'ex-id)
                                            (datum->syntax #f 'x)))
                       (define i (make-syntax-introducer))
                       (let* ([req (if just-space #`(just-space #,just-space #,req) req)]
                              [req (if for-space #`(for-space #,for-space #,req) req)]
                              [req (if only-phase #`(just-meta #,only-phase #,req) req)]
                              [req (if (eqv? phase-level 0) req #`(for-meta #,phase-level #,req))])
                         #`(begin
                             (#%require #,(i req))
                             (provide (all-from-out #,(i mp)))))]
                      [else
                       #`(begin)]))
                  (syntax-parse req
                    #:datum-literals (for-meta for-syntax for-template for-label just-meta portal
                                               for-space just-space
                                               rename only prefix all-except prefix-all-except)
                    [(for-meta at-phase-level req ...)
                     (seq-loop #'(req ...) (syntax-e #'at-phase-level) only-phase just-space for-space)]
                    [(for-syntax req ...) (seq-loop #'(req ...) 1 only-phase just-space for-space)]
                    [(for-template req ...) (seq-loop #'(req ...) -1 only-phase just-space for-space)]
                    [(for-label req ...) (seq-loop #'(req ...) #f only-phase just-space for-space)]
                    [(just-meta at-phase-level req ...)
                     (seq-loop #'(req ...) phase-level #'at-phase-level-space for-space)]
                    [(just-space space req ...)
                     (seq-loop #'(req ...) phase-level only-phase #'space for-space)]
                    [(for-space space req ...)
                     (seq-loop #'(req ...) phase-level only-phase just-space #'space)]
                    [(portal id . _) (make-maybe-provide #'id)]
                    [(rename _ id _) (make-maybe-provide #'id)]
                    [(only _ id ...) #`(begin
                                         #,@(for/list ([id (in-list (syntax->list #'(id ...)))])
                                              (make-maybe-provide (datum->syntax req (syntax-e id) id id))))]
                    [(prefix _ mp) (reprovide #'mp)]
                    [(all-except mp . _) (reprovide #'mp)]
                    [(prefix-all-except _ mp . _) (reprovide #'mp)]
                    [_ (reprovide req)])))))]))
