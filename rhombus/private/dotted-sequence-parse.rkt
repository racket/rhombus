#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     enforest/syntax-local
                     "name-path-op.rkt"
                     "operator-parse.rkt"
                     "introducer.rkt"
                     "srcloc.rkt")
         "expression.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "parens.rkt")

(begin-for-syntax
  (provide :dotted-identifier-sequence
           :dotted-operator-or-identifier-sequence
           :dotted-identifier
           :dotted-operator-or-identifier
           build-definitions/maybe-extension
           build-syntax-definition/maybe-extension
           identifier-extension-binding?))

;; A dotted identifier as a bindig form does not go though `:dotted-identifier-sequence`.
;; Instead, suitable binding information is created by `name-root-ref` via the
;; `binding-extension-combine` argument.

(begin-for-syntax
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
      [(null? head-ids) (values tail-id #'#f)]
      [(extensible-name-root head-ids)
       => (lambda (extends-id)
            (values (datum->syntax tail-id
                                   (build-dot-symbol (append head-ids (list tail-id)))
                                   tail-id
                                   tail-id)
                    extends-id))]
      [else (raise-syntax-error (build-dot-symbol head-ids)
                                "not defined as a namespace"
                                all)]))

  (define-syntax-class :dotted-identifier
    #:attributes (name extends)
    #:datum-literals (op |.|)
    (pattern (~and all ((~seq head-id:identifier (op |.|)) ... tail-id:identifier))
             #:do [(define-values (name extends) (build-dot-identifier #'(head-id ...) #'tail-id #'all))]
             #:attr name name
             #:attr extends extends))

  (define-syntax-class :dotted-operator-or-identifier
    #:attributes (name extends)
    #:datum-literals (op |.|)
    (pattern ((op o))
             #:attr name #'o
             #:attr extends #'#f)
    (pattern (~and all ((~seq head-id:identifier (op |.|)) ... (parens (group (op tail-op)))))
             #:do [(define-values (name extends) (build-dot-identifier #'(head-id ...) #'tail-op #'all))]
             #:attr name name
             #:attr extends extends)
    (pattern id::dotted-identifier
             #:attr name #'id.name
             #:attr extends #'id.extends)))

(begin-for-syntax
  (struct extension-rename-transformer (id extends-id)
    #:property prop:rename-transformer 0))

(define-for-syntax (build-definitions/maybe-extension space-sym name-in extends rhs)
  (define name ((space->introducer space-sym) name-in))
  (cond
    [(syntax-e extends)
     (define tmp ((space->introducer space-sym) (car (generate-temporaries (list name)))))
     (list
      #`(define #,tmp (let ([#,name #,rhs])
                        #,name))
      #`(define-syntax #,name (extension-rename-transformer (quote-syntax #,tmp)
                                                            (quote-syntax #,extends))))]
    [else
     (list
      #`(define #,name #,rhs))]))

(define-for-syntax (build-syntax-definition/maybe-extension space-sym name-in extends rhs)
  (define name ((space->introducer space-sym) name-in))
  (cond
    [(syntax-e extends)
     (define tmp ((space->introducer space-sym) (car (generate-temporaries (list name)))))
     #`(begin
         (define-syntax #,tmp (let ([#,name #,rhs])
                                #,name))
         (define-syntax #,name (extension-rename-transformer (quote-syntax #,tmp)
                                                             (quote-syntax #,extends))))]
    [else
     #`(define-syntax #,name #,rhs)]))

(define-for-syntax (identifier-extension-binding? id prefix)
  (define v (syntax-local-value* id (lambda (v)
                                      (and (extension-rename-transformer? v)
                                           v))))
  (and v
       (free-identifier=? prefix (extension-rename-transformer-extends-id v))))
