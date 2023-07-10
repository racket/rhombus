#lang racket/base
(require syntax/stx
         "../proc-name.rkt")

(provide transform-in
         transform-out
         call-as-transformer
         check-transformer-result
         track-sequence-origin)

(define no-props (datum->syntax #f #f))

(define current-transformer-introduce (make-parameter (lambda (stx) stx)))
(define (transform-in stx)
  ((current-transformer-introduce) stx))
(define (transform-out stx)
  ((current-transformer-introduce) stx))

(define (call-as-transformer id track-origin thunk)
  (define intro (make-syntax-introducer))
  (parameterize ([current-transformer-introduce intro])
    (thunk intro
           (lambda (stx)
             (let loop ([stx stx])
               (cond
                 [(syntax? stx)
                  (track-origin (intro stx)
                                (let ([du (syntax-property id 'disappeared-use)])
                                  (if du
                                      (syntax-property no-props 'disappeared-use du)
                                      no-props))
                                id)]
                 [(pair? stx) (cons (loop (car stx))
                                    (loop (cdr stx)))]
                 [else stx]))))))

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
                 (for/list ([e (syntax->list stx)])
                   (syntax-track-origin e from-stx id))
                 stx
                 stx))

