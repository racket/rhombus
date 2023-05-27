#lang racket/base
(require syntax/parse/pre
         "operator-parse.rkt"
         (for-template "parens.rkt"))

(provide :dotted-identifier-sequence
         :dotted-operator-or-identifier-sequence
         build-dot-symbol)

(define-splicing-syntax-class :dotted-identifier-sequence
  (pattern (~seq head-id:identifier (~seq _::op-dot tail-id:identifier) ...)))

(define-splicing-syntax-class :dotted-operator-or-identifier-sequence
  #:datum-literals (group)
  (pattern (~seq _::operator))
  (pattern (~seq (~seq _:identifier _::op-dot) ... _:identifier))
  (pattern (~seq (~seq _:identifier _::op-dot) ...+ (_::parens (group _::operator)))))

(define (build-dot-symbol ids #:skip-dots? [skip-dots? #f])
  (string->symbol
   (apply string-append
          (let loop ([ids ids])
            (cond
              [(null? (cdr ids)) (list (symbol->string
                                        (let ([s (syntax-e (car ids))])
                                          (if (and skip-dots?
                                                   (pair? s))
                                              ;; must be `(op)`
                                              (syntax-e
                                               (cadr
                                                (syntax->list
                                                 (cadr
                                                 (syntax->list
                                                  (cadr
                                                   (syntax->list (car ids))))))))
                                              s))))]
              [else (list* (symbol->string (syntax-e (car ids)))
                           "."
                           (loop (if skip-dots? (cddr ids) (cdr ids))))])))))

