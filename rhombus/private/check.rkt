#lang racket/base
(require syntax/stx)

(provide check-expression-result
         check-binding-result
         check-transformer-result

         proc-name)

(define (check-expression-result form proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  form)

(define (check-binding-result var-ids matcher-form stx-ids stx-form proc)
  (define (check-ids ids)
    (unless (and (stx-list? ids)
                 (let loop ([ids ids])
                   (or (stx-null? ids)
                       (and (stx-pair? ids)
                            (identifier? (stx-car ids))
                            (loop (stx-cdr ids))))))
      (raise-result-error (proc-name proc) "(listof identifier?))" ids)))
  (check-ids var-ids)
  (unless (syntax? matcher-form) (raise-result-error (proc-name proc) "syntax?" matcher-form))
  (check-ids stx-ids)
  (unless (syntax? stx-form) (raise-result-error (proc-name proc) "syntax?" stx-form))
  (datum->syntax #f (list var-ids matcher-form stx-ids stx-form)))

(define (check-transformer-result form tail proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  (unless (stx-list? tail) (raise-result-error (proc-name proc) "stx-list?" tail))
  (values form tail))

(define (proc-name proc)
  (define s (object-name proc))
  (if (symbol? s)
      s
      'transformer-procedure))
