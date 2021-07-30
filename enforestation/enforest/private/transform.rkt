#lang racket/base
(require syntax/stx
         "../proc-name.rkt")

(provide transform-in
         transform-out
         call-as-transformer
         check-transformer-result)

(define no-props (datum->syntax #f #f))

(define current-transformer-introduce (make-parameter (lambda (stx) stx)))
(define (transform-in stx)
  ((current-transformer-introduce) stx))
(define (transform-out stx)
  ((current-transformer-introduce) stx))

(define (call-as-transformer id thunk)
  (define intro (make-syntax-introducer))
  (parameterize ([current-transformer-introduce intro])
    (thunk intro
           (lambda (stx)
             (let loop ([stx stx])
               (cond
                 [(syntax? stx)
                  (syntax-track-origin (intro stx)
                                       no-props
                                       id)]
                 [(pair? stx) (cons (loop (car stx))
                                    (loop (cdr stx)))]
                 [else stx]))))))

(define (check-transformer-result form tail proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  (unless (stx-list? tail) (raise-result-error (proc-name proc) "stx-list?" tail))
  (values form tail))
