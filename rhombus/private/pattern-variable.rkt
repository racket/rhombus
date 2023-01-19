#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     "srcloc.rkt")
         syntax/parse/pre
         "parse.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         (submod "syntax-class-primitive.rkt" for-quasiquote)
         "dollar.rkt"
         "repetition.rkt"
         "static-info.rkt"
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-syntax make-pattern-variable-syntax))

(define-for-syntax (make-pattern-variable-syntax name-id temp-id unpack* depth splice? attributes)
  (define (lookup-attribute stx var-id attr-id want-repet?)
    (define attr (for/or ([var (in-list (syntax->list attributes))])
                   (and (eq? (syntax-e attr-id) (syntax-e (car (syntax-e var))))
                        (syntax-list->pattern-variable var))))
    (unless (and attr (eq? want-repet? (not (eqv? 0 (+ depth
                                                       (if splice? -1 0)
                                                       (pattern-variable-depth attr))))))
      (raise-syntax-error #f
                          (format
                           (string-append (if attr
                                              (if want-repet?
                                                  "attribute is not a repetition\n"
                                                  "attribute is a repetition\n")
                                              "attribute not found\n")
                                          "  pattern: ~a\n"
                                          "  attribute: ~a")
                           (syntax-e var-id)
                           (syntax-e attr-id))
                          stx))
    attr)
  (define expr-handler
    (lambda (stx fail)
      (syntax-parse stx
        #:datum-literals (op |.|)
        [(var-id (op |.|) attr-id . tail)
         (define attr (lookup-attribute stx #'var-id #'attr-id #f))
         (values (wrap-static-info* (pattern-variable-val-id attr)
                                    syntax-static-infos)
                 #'tail)]
        [_
         (if (eqv? depth 0)
             (id-handler stx)
             (fail))])))
  (define id-handler
    (lambda (stx)
      (syntax-parse stx
        [(_ . tail) (values (wrap-static-info* temp-id syntax-static-infos) #'tail)])))
  (cond
    [(and (eqv? 0 depth)
          (for/and ([a (in-list (syntax->list attributes))])
            (eqv? 0 (pattern-variable-depth (syntax-list->pattern-variable a)))))
     (if (null? (syntax-e attributes))
         (expression-transformer
          name-id
          id-handler)
         (expression-transformer
          name-id
          (lambda (stx)
            (expr-handler stx
                          (lambda ()
                            (id-handler stx))))))]
    [else (make-repetition
           name-id
           #`(#,unpack* #'$ #,temp-id #,depth)
           syntax-static-infos
           #:depth depth
           #:repet-handler (lambda (stx next)
                             (syntax-parse stx
                               #:datum-literals (op |.|)
                               [(var-id (~and dot-op (op |.|)) attr-id . tail)
                                (define attr (lookup-attribute stx #'var-id #'attr-id #t))
                                (define var-depth (+ (pattern-variable-depth attr) depth (if splice? -1 0)))
                                (values (make-repetition-info #'(var-id dot-op attr-id)
                                                              (string->symbol
                                                               (format "~a.~a" (syntax-e #'var-id) (syntax-e #'attr-id)))
                                                              #`(#,(pattern-variable-unpack*-id attr)
                                                                 #'$
                                                                 #,(pattern-variable-val-id attr)
                                                                 #,var-depth)
                                                              var-depth
                                                              #'0
                                                              syntax-static-infos
                                                              #f)
                                        #'tail)]
                               [_ (next)]))
           #:expr-handler expr-handler)]))
