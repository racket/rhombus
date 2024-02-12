#lang racket/base
(require racket/base
         syntax/stx
         "property.rkt"
         "syntax-local.rkt"
         "proc-name.rkt"
         "private/transform.rkt")

(provide (property-out name-root)
         name-root-proc)

(module+ for-parse
  (provide apply-name-root
           name-root-ref-root))

(property name-root (proc))
  
(define (apply-name-root op-stx lxc in-space stxes)
  (define proc (name-root-proc lxc))
  (define-values (target tail)
    (call-as-transformer
     op-stx
     (list stxes)
     syntax-track-origin
     (lambda (stxes)
       (define-values (target tail) (proc in-space stxes))
       (unless (or (identifier? target)
                   (and (syntax? target)
                        (pair? (syntax-e target))
                        (eq? 'op (syntax-e (car (syntax-e target))))))
         (raise-result-error (proc-name proc) "identifier-or-operator?" target))
       (values target tail))))
  (check-transformer-result target tail proc))

(define (name-root-ref-root v ref)
  (ref v))
