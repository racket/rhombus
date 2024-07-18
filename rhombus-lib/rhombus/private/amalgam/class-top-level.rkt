#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (only-in (submod "annotation.rkt" for-class)
                  define-annotation-syntax
                  define-annotation-constructor)
         (only-in "binding.rkt"
                  define-binding-syntax)
         (only-in (submod "dot.rkt" for-dot-provider)
                  define-dot-provider-syntax)
         (only-in (submod "with.rkt" for-update)
                  define-update-syntax)
         (only-in "name-root.rkt"
                  define-name-root)
         (only-in "class-desc.rkt"
                  define-class-desc-syntax)
         (only-in "class-define-method-result.rkt"
                  define-method-result))

(provide (for-syntax top-level-declare
                     reorder-for-top-level))

(define-for-syntax (top-level-declare ids-stx)
  (cond
    [(eq? (syntax-local-context) 'top-level)
     ;; avoid duplicate definitions here; they'll get reported by later checking
     (define (remove-dups stx)
       (let loop ([ids (syntax->list stx)] [seen #hasheq()])
         (cond
           [(null? ids) null]
           [(hash-ref seen (syntax-e (car ids)) #f) (loop (cdr ids) seen)]
           [else (cons (car ids)
                       (loop (cdr ids) (hash-set seen (syntax-e (car ids)) #t)))])))
     (list #`(define-syntaxes #,(remove-dups ids-stx) (values)))]
    [else null]))

(define-for-syntax (reorder-for-top-level defs)
  (cond
    [(eq? (syntax-local-context) 'top-level)
     (let loop ([defs defs] [stxs '()] [vars '()] [var-names '()])
       (cond
         [(null? defs)
          (append
           (list #`(define-syntaxes #,var-names (values)))
           (reverse stxs)
           (reverse vars))]
         [else
          (define def (car defs))
          (syntax-parse def
            #:literals (begin
                        define define-values
                        define-syntax define-syntaxes
                        define-class-desc-syntax
                        define-name-root
                        define-binding-syntax
                        define-annotation-syntax define-annotation-constructor
                        define-dot-provider-syntax
                        define-update-syntax
                        define-method-result)
            [(begin d ...)
             (loop (append (syntax->list #'(d ...)) (cdr defs)) stxs vars var-names)]
            [(~or* (define (id . _) . _)
                   (define id _))
             (loop (cdr defs) stxs (cons def vars) (cons #'id var-names))]
            [(define-values (id ...) _)
             (loop (cdr defs) stxs (cons def vars) (append (syntax->list #'(id ...)) var-names))]
            [(~or* (define-syntax . _)
                   (define-syntaxes . _)
                   (define-class-desc-syntax . _)
                   (define-name-root . _)
                   (define-binding-syntax . _)
                   (define-annotation-syntax . _)
                   (define-annotation-constructor . _)
                   (define-dot-provider-syntax . _)
                   (define-update-syntax . _)
                   (define-method-result . _))
             (loop (cdr defs) (cons def stxs) vars var-names)]
            [_
             (loop (cdr defs) stxs (cons def vars) var-names)])]))]
    [else defs]))
