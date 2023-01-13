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
         (submod "syntax-class.rkt" for-quasiquote)
         "dollar.rkt"
         "repetition.rkt"
         "static-info.rkt"
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-syntax make-pattern-variable-syntax))

(define-for-syntax (make-pattern-variable-syntax name-id temp-id unpack* depth splice? attributes)
  (define (lookup-attribute stx var-id attr-id want-repet?)
    (define attr (hash-ref attributes (syntax->datum attr-id) #f))
    (unless (and attr (eq? want-repet? (not (eqv? 0 (+ depth
                                                       (if splice? -1 0)
                                                       (syntax-class-attribute-depth attr))))))
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
         (values (wrap-static-info* (syntax-class-attribute-id attr)
                                    syntax-static-infos)
                 #'tail)]
        [_ (fail)])))
  (define id-handler
    (lambda (stx)
      (syntax-parse stx
        [(_ . tail) (values (wrap-static-info* temp-id syntax-static-infos) #'tail)])))
  (cond
    [(eq? depth 0) (if (eq? 0 (hash-count attributes))
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
                                (values (make-repetition-info #'(var-id dot-op attr-id)
                                                              (string->symbol
                                                               (format "~a.~a" (syntax-e #'var-id) (syntax-e #'attr-id)))
                                                              (syntax-class-attribute-id attr)
                                                              (+ (syntax-class-attribute-depth attr) depth (if splice? -1 0))
                                                              #'0
                                                              syntax-static-infos
                                                              #f)
                                        #'tail)]
                               [_ (next)]))
           #:expr-handler expr-handler)]))
