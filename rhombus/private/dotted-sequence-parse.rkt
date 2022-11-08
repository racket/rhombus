#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "name-path-op.rkt"
                     "operator-parse.rkt")
         "name-root-ref.rkt"
         "parens.rkt")

(begin-for-syntax
  (provide :dotted-identifier-sequence
           :dotted-operator-or-identifier-sequence
           :dotted-identifier
           :dotted-operator-or-identifier
           build-dot-identifier))

(begin-for-syntax
  (define-syntax-class :op-dot
    #:description "a dot operator"
    #:datum-literals (op |.|)
    (pattern (op |.|)))
  
  (define-splicing-syntax-class :dotted-identifier-sequence
    (pattern (~seq head-id:identifier (~seq _::op-dot tail-id:identifier) ...)))

  (define-splicing-syntax-class :dotted-operator-or-identifier-sequence
    #:datum-literals (group)
    (pattern (~seq _::operator))
    (pattern (~seq (~seq _:identifier _::op-dot) ... _:identifier))
    (pattern (~seq (~seq _:identifier _::op-dot) ... (_::parens (group _::operator)))))

  (define (build-dot-symbol ids)
    (string->symbol
     (apply string-append
            (let loop ([ids ids])
              (cond
                [(null? (cdr ids)) (list (symbol->string (syntax-e (car ids))))]
                [else (list* (symbol->string (syntax-e (car ids)))
                             "."
                             (loop (cdr ids)))])))))

  (define (build-dot-identifier head-ids-stx tail-id all)
    (define head-ids (syntax->list head-ids-stx))
    (cond
      [(null? head-ids) tail-id]
      [(extensible-name-root? head-ids)
       (datum->syntax tail-id
                      (build-dot-symbol (append head-ids (list tail-id)))
                      tail-id
                      tail-id)]
      [else (raise-syntax-error (build-dot-symbol head-ids)
                                "not defined as a namespace"
                                all)]))

  (define-syntax-class :dotted-identifier
    #:datum-literals (op |.|)
    (pattern (~and all ((~seq head-id:identifier (op |.|)) ... tail-id:identifier))
             #:do [(define name (build-dot-identifier #'(head-id ...) #'tail-id #'all))]
             #:attr name name))

  (define-syntax-class :dotted-operator-or-identifier
    #:datum-literals (op |.|)
    (pattern ((op o))
             #:attr name #'o)
    (pattern (~and all ((~seq head-id:identifier (op |.|)) ... (parens (group (op tail-op)))))
             #:do [(define name (build-dot-identifier #'(head-id ...) #'tail-op #'all))]
             #:attr name name)
    (pattern id::dotted-identifier
             #:attr name #'id.name)))
