#lang racket/base
(require racket/symbol
         syntax/parse/pre
         "operator-parse.rkt"
         "parens-sc.rkt")

(provide :dotted-identifier-sequence
         :dotted-operator-or-identifier-sequence
         :raw-dotted-operator-or-identifier
         build-dot-symbol)

(define-splicing-syntax-class :dotted-identifier-sequence
  #:description "dotted identifier sequence"
  #:opaque
  (pattern (~seq head-id:identifier (~seq _::op-dot tail-id:identifier) ...)))

(define-splicing-syntax-class :dotted-operator-or-identifier-sequence
  #:description "dotted name sequence"
  #:opaque
  #:datum-literals (group)
  (pattern (~seq _::operator))
  (pattern (~seq (~seq _:identifier _::op-dot) ... _:identifier))
  (pattern (~seq (~seq _:identifier _::op-dot) ...+ (_::parens (group _::operator)))))

(define-syntax-class :raw-dotted-operator-or-identifier
  #:attributes (prefix name)
  #:datum-literals (group)
  (pattern (op::operator)
           #:attr name #'op.name
           #:attr prefix #'())
  (pattern ((~seq prefix-elem:identifier _::op-dot) ... name:identifier)
           #:attr prefix #'(prefix-elem ...))
  (pattern ((~seq prefix-elem:identifier _::op-dot) ...+ (_::parens (group op::operator)))
           #:attr name #'op.name
           #:attr prefix #'(prefix-elem ...)))

(define (build-dot-symbol ids #:skip-dots? [skip-dots? #f])
  (if (null? (cdr ids))
      ;; immediate identifer or operator
      (let ([s (syntax-e (car ids))])
        (if (pair? s)
            ;; must be operator
            (syntax-e (cadr (syntax->list (car ids))))
            s))
      ;; sequence of identifiers ending with identifier or parenthesized operator
      (string->symbol
       (apply string-append
              (let loop ([ids ids])
                (cond
                  [(null? (cdr ids)) (list (symbol->immutable-string
                                            (let ([s (syntax-e (car ids))])
                                              (if (and skip-dots?
                                                       (pair? s))
                                                  ;; must be parenthesized operator
                                                  (syntax-e
                                                   (cadr
                                                    (syntax->list
                                                     (cadr
                                                      (syntax->list
                                                       (cadr
                                                        (syntax->list (car ids))))))))
                                                  s))))]
                  [else (list* (symbol->immutable-string (syntax-e (car ids)))
                               "."
                               (loop (if skip-dots? (cddr ids) (cdr ids))))]))))))
