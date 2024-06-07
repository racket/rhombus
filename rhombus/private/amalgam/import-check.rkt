#lang racket/base
(require syntax/parse/pre
         racket/phase+space)

;; `check-require-bindings` is used to check for duplicate imports, but
;; specificaly ones that are duplicate with respect to `let`

(provide check-require-bindings)

(define (check-require-bindings req sub)
  (define top-req req)
  (define (check-identifier id phase space)
    (when (and (identifier-binding id)
               (identifier-distinct-binding id (sub id)))
      (raise-syntax-error #f "duplicate import" id)))
  (let loop ([req req]             
             [phase-shift (syntax-local-phase-level)]
             [just-phase #f]
             [for-space #f]
             [just-space #f]
             [excepts #hasheq()])
    (syntax-parse req
      #:datum-literals (portal
                        for-meta for-syntax for-template for-label just-meta
                        for-space just-space
                        only prefix
                        all-except prefix-all-except
                        rename)
      [(portal id content) (check-identifier #'id phase-shift for-space)]
      [(for-syntax req ...)
       (for ([req (in-list (syntax->list #'(req ...)))])
         (loop req (+ phase-shift 1) just-phase for-space just-space excepts))]
      [(for-template req ...)
       (for ([req (in-list (syntax->list #'(req ...)))])
         (loop req (- phase-shift 1) just-phase for-space just-space excepts))]
      [(for-meta phase-level req ...)
       (when (syntax-e #'phase-level)
         (for ([req (in-list (syntax->list #'(req ...)))])
           (loop req (+ phase-shift (syntax-e #'phase-level)) just-phase for-space just-space excepts)))]
      [(just-meta phase-level req ...)
       (let ([new-just-phase (syntax-e #'phase-level)])
         (when (or (not just-phase)
                   (equal? just-phase new-just-phase))
           (for ([req (in-list (syntax->list #'(req ...)))])
             (loop req phase-shift new-just-phase for-space just-space excepts))))]
      [(for-space space req ...)
       (for ([req (in-list (syntax->list #'(req ...)))])
         (loop req phase-shift just-phase (syntax-e #'space) just-space excepts))]
      [(just-space space req ...)
       (unless (or (not just-space)
                   (eq? just-space (syntax-e #'space)))
         (for ([req (in-list (syntax->list #'(req ...)))])
           (loop req phase-shift just-phase for-space (syntax-e #'just-space) excepts)))]
      [(only _ id ...)
       (for ([id (in-list (syntax->list #'(id ...)))])
         (check-identifier id phase-shift for-space))]
      [(rename _ id _)
       (check-identifier #'id phase-shift for-space)]
      [(prefix . _)
       (raise-syntax-error #f "not supported" req)]
      [(all-except raw-module-path id ...)
       (loop #'raw-module-path phase-shift just-phase for-space just-space
             (for/fold ([excepts excepts]) ([id (in-list (syntax->list #'(id ...)))])
               (hash-set excepts (syntax-e id) #t)))]
      [(prefix-all-except . _)
       (raise-syntax-error #f "not supported" req)]
      [_
       ;; module path
       (define phase+space+symss (syntax-local-module-exports (syntax->datum req)))
       (for ([phase+space+syms (in-list phase+space+symss)]
             #:do [(define phase (phase+space-phase (car phase+space+syms)))
                   (define space (phase+space-space (car phase+space+syms)))]
             #:when (or (not just-space) (eq? just-space space))
             #:when (or (not just-phase) (eq? just-phase phase))
             [sym (in-list (cdr phase+space+syms))])
         (define space (phase+space-space (car phase+space+syms)))
         (check-identifier (datum->syntax top-req sym) (+ phase phase-shift) (or for-space space)))])))
