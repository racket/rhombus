#lang racket/base
(require racket/base
         syntax/stx
         "property.rkt"
         "syntax-local.rkt"
         "proc-name.rkt"
         "private/transform.rkt")

(provide (property-out lexicon)
         lexicon-proc)

(module+ for-parse
  (provide apply-lexicon))

(property lexicon (proc))
  
(define (apply-lexicon op-stx lxc stxes)
  (define proc (lexicon-proc lxc))
  (call-as-transformer
   op-stx
   (lambda (in out)
     (define-values (target tail) (proc (in stxes)))
     (unless (or (identifier? target)
                 (and (syntax? target)
                      (pair? (syntax-e target))
                      (eq? 'op (syntax-e (car (syntax-e target))))))
       (raise-result-error (proc-name proc) "identifier-or-operator?" target))
     (check-transformer-result (out target) (out tail) proc))))
