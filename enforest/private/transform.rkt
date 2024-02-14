#lang racket/base
(require syntax/stx
         "../proc-name.rkt")

(provide transform-in
         transform-out
         call-as-transformer
         check-transformer-result
         track-sequence-origin)

(define no-props (datum->syntax #f #f))

(define (transform-in stx)
  (syntax-local-introduce stx))
(define (transform-out stx)
  (syntax-local-introduce stx))

(define (call-as-transformer id args track-origin use-site-scopes? proc)
  (call-with-values
   (lambda ()
     (apply syntax-local-apply-transformer
            proc
            id
            (cond
              [use-site-scopes?
               (define context (syntax-local-context))
               (if (eq? context 'expression)
                   (list (gensym)) ; to inherit use-site-scope context
                   context)]
              [else
               ;; use contexts that imply no use-site scopes:
               (if (eq? 'top-level (syntax-local-context))
                   'top-level
                   'expression)])
            #f
            (map syntax-local-introduce args)))
   (lambda stxes
     (apply values (map syntax-local-introduce (map (track track-origin id) stxes))))))

(define ((track track-origin id) stx)
  (track-origin stx
                (let ([du (syntax-property id 'disappeared-use)])
                  (if du
                      (syntax-property no-props 'disappeared-use du)
                      no-props))
                id))

(define (check-transformer-result form tail proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  ;; we'd like to check for a syntax list in `tail`, but that's not constant-time
  (unless (or (pair? tail)
              (null? tail)
              (and (syntax? tail)
                   (let ([e (syntax-e tail)])
                     (or (pair? e) (null? e)))))
    (raise-result-error (proc-name proc) "stx-list?" tail))
  (values form tail))

(define (track-sequence-origin stx from-stx id)
  (datum->syntax stx
                 (for/list ([e (in-list (syntax->list stx))])
                   (syntax-track-origin e from-stx id))
                 stx
                 stx))
