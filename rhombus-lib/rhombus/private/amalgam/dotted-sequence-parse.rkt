#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "dotted-sequence.rkt"
                     "introducer.rkt")
         "name-root-ref.rkt")

(begin-for-syntax
  (provide :dotted-identifier-sequence
           :dotted-operator-or-identifier-sequence
           :dotted-identifier
           :dotted-operator-or-identifier
           build-definitions/maybe-extension
           build-syntax-definition/maybe-extension
           build-syntax-definitions/maybe-extension
           identifier-extension-binding?))

;; A dotted identifier as a binding form does not go though `:dotted-identifier-sequence`.
;; Instead, suitable binding information is created by `name-root-ref` via the
;; `binding-extension-combine` argument.

(begin-for-syntax
  (define (build-dot-identifier head-ids-stx tail-id all)
    (define head-ids (syntax->list head-ids-stx))
    (cond
      [(null? head-ids) (values tail-id #'#f tail-id)]
      [(extensible-name-root head-ids)
       => (lambda (extends-id)
            (values (datum->syntax tail-id
                                   (build-dot-symbol (append head-ids (list tail-id)))
                                   tail-id
                                   tail-id)
                    extends-id
                    tail-id))]
      [else (raise-syntax-error (build-dot-symbol head-ids)
                                "not defined as a namespace"
                                all)]))

  (define-syntax-class :dotted-identifier
    #:description "dotted identifier"
    #:attributes (name extends tail-name)
    #:datum-literals (op |.|)
    (pattern (~and all ((~seq head-id:identifier (op |.|)) ... tail-id:identifier))
             #:do [(define-values (name extends tail-name) (build-dot-identifier #'(head-id ...) #'tail-id #'all))]
             #:with name name
             #:with extends extends
             #:with tail-name tail-name))

  (define-syntax-class :dotted-operator-or-identifier
    #:description "dotted operator or identifier"
    #:attributes (name extends tail-name)
    #:datum-literals (parens group op |.|)
    (pattern ((op (~and name tail-name)))
             #:with extends #'#f)
    (pattern (~and all ((~seq head-id:identifier (op |.|)) ... (parens (group (op tail-op)))))
             #:do [(define-values (name extends tail-name) (build-dot-identifier #'(head-id ...) #'tail-op #'all))]
             #:with name name
             #:with extends extends
             #:with tail-name tail-name)
    (pattern ::dotted-identifier)))

(begin-for-syntax
  (struct extension-rename-transformer (id extends-id)
    #:property prop:rename-transformer 0))

(define-for-syntax (build-definitions/maybe-extension space-sym name-in extends rhs)
  (define name ((space->introducer space-sym) name-in))
  (cond
    [(syntax-e extends)
     (define tmp ((space->introducer space-sym)
                  ((make-syntax-introducer)
                   (datum->syntax #f (syntax-e name) name))))
     (list
      #`(define #,tmp #,rhs)
      #`(define-syntax #,name (extension-rename-transformer (quote-syntax #,tmp)
                                                            (quote-syntax #,extends))))]
    [else
     (list
      #`(define #,name #,rhs))]))

(define-for-syntax (build-syntax-definition/maybe-extension space-sym name-in extends rhs)
  (define intro (space->introducer space-sym))
  (define name (intro name-in))
  (cond
    [(syntax-e extends)
     (define tmp (intro (car (generate-temporaries (list name)))))
     #`(begin
         (define-syntax #,tmp (let ([#,name #,rhs])
                                #,name))
         (define-syntax #,name (extension-rename-transformer (quote-syntax #,tmp)
                                                             (quote-syntax #,extends))))]
    [else
     #`(define-syntax #,name #,rhs)]))

(define-for-syntax (build-syntax-definitions/maybe-extension space-syms name-in extends rhs
                                                             #:extra-names [extra-names null])
  (define names (for/list ([space-sym (in-list space-syms)])
                  ((space->introducer space-sym) name-in)))
  (cond
    [(syntax-e extends)
     (define tmps (for/list ([space-sym (in-list space-syms)])
                    ((space->introducer space-sym) (car (generate-temporaries (list name-in))))))
     (cons
      #`(define-syntaxes (#,@tmps #,@extra-names)
          (let-values ([(#,@names #,@extra-names) #,rhs])
            (values #,@names #,@extra-names)))
      (for/list ([name (in-list names)]
                 [tmp (in-list tmps)])
        #`(define-syntax #,name (extension-rename-transformer (quote-syntax #,tmp)
                                                              (quote-syntax #,extends)))))]
    [else
     (list
      #`(define-syntaxes (#,@names #,@extra-names) #,rhs))]))

(define-for-syntax (identifier-extension-binding? id prefix)
  (define v (syntax-local-value* id (lambda (v)
                                      (and (or (extension-rename-transformer? v)
                                               (portal-syntax? v))
                                           v))))
  (cond
    [(extension-rename-transformer? v)
     (free-identifier=? prefix (extension-rename-transformer-extends-id v))]
    [(portal-syntax? v)
     (define extends (portal-syntax->extends (portal-syntax-content v)))
     (and (identifier? extends)
          (free-identifier=? prefix extends))]
    [else #f]))
