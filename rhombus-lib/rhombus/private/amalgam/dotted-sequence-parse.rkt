#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/property
                     enforest/syntax-local
                     "dotted-sequence.rkt"
                     "introducer.rkt"
                     "srcloc.rkt")
         "name-root-ref.rkt")

(begin-for-syntax
  (provide :dotted-identifier-sequence
           :dotted-operator-or-identifier-sequence
           :dotted-identifier
           :dotted-operator-or-identifier
           build-definitions/maybe-extension
           build-syntax-definition/maybe-extension
           build-syntax-definitions/maybe-extension
           identifier-extension-binding?
           identifier-extension-binding-tail-name))

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
    #:property prop:rename-transformer 0)
  (define (id-as-ext-target tmp)
    (syntax-property tmp 'not-free-identifier=? #t)))

(define-for-syntax (build-definitions/maybe-extension space-sym name-in extends rhs
                                                      #:simple-rhs? [simple-rhs? #f])
  (define name ((space->introducer space-sym) name-in))
  (cond
    [(syntax-e extends)
     (define (make-rhs tmp)
       #`(define-syntax #,name (extension-rename-transformer (quote-syntax #,(id-as-ext-target tmp))
                                                             (quote-syntax #,extends))))
     (cond
       [simple-rhs?
        (list (make-rhs rhs))]
       [else
        (define tmp ((space->introducer space-sym)
                     ((make-syntax-introducer)
                      (datum->syntax #f (syntax-e name) name))))
        (list
         #`(define #,tmp #,rhs)
         (make-rhs tmp))])]
    [else
     (list
      #`(define #,name #,rhs))]))

(define-for-syntax (build-syntax-definition/maybe-extension space-sym name-in extends rhs
                                                            #:form [form-stx-or-string "def"])
  (define intro (space->introducer space-sym))
  (define name (intro name-in))
  (with-syntax ([define-syntaxes (syntax-raw-property #'define-syntaxes (extract-form-name form-stx-or-string))])
    (cond
      [(syntax-e extends)
       (define tmp (intro (car (generate-temporaries (list name)))))
       #`(begin
           #,((make-relocate-to-form form-stx-or-string)
              #`(define-syntaxes (#,tmp) (let ([#,name #,rhs])
                                           #,name)))
           #,((make-relocate-to-form form-stx-or-string)
              #`(define-syntaxes (#,name) (extension-rename-transformer (quote-syntax #,(id-as-ext-target tmp))
                                                                        (quote-syntax #,extends)))))]
      [else
       ((make-relocate-to-form form-stx-or-string)
        #`(define-syntaxes (#,name) #,rhs))])))

(define-for-syntax (build-syntax-definitions/maybe-extension space-syms name-in extends rhs
                                                             #:extra-names [extra-names null]
                                                             #:form [form-stx-or-string "def"])
  (define names (for/list ([space-sym (in-list space-syms)])
                  ((space->introducer space-sym) name-in)))
  (with-syntax ([define-syntaxes (syntax-raw-property #'define-syntaxes (extract-form-name form-stx-or-string))])
    (cond
      [(syntax-e extends)
       (define tmps (for/list ([space-sym (in-list space-syms)])
                      ((space->introducer space-sym) (car (generate-temporaries (list name-in))))))
       (map
        (make-relocate-to-form form-stx-or-string)
        (cons
         #`(define-syntaxes (#,@tmps #,@extra-names)
             (let-values ([(#,@names #,@extra-names) #,rhs])
               (values #,@names #,@extra-names)))
         (for/list ([name (in-list names)]
                    [tmp (in-list tmps)])
           #`(define-syntaxes (#,name) (extension-rename-transformer (quote-syntax #,(id-as-ext-target tmp))
                                                                     (quote-syntax #,extends))))))]
      [else
       (list
        ((make-relocate-to-form form-stx-or-string)
         #`(define-syntaxes (#,@names #,@extra-names) #,rhs)))])))

(define-for-syntax (extract-form-name form-name)
  (cond
    [(string? form-name) form-name]
    [(identifier? form-name) (or (syntax-raw-property form-name)
                                 (format "~a" (syntax-e form-name)))]
    [else
     (syntax-parse form-name
       [(name:id . _) (extract-form-name #'name)]
       [_ "def"])]))

(define-for-syntax (make-relocate-to-form form-stx-or-string)
  (if (syntax? form-stx-or-string)
      (lambda (stx)
        (relocate+reraw form-stx-or-string stx))
      values))

(define-for-syntax (identifier-extension-binding? id prefix)
  (syntax-local-value* id (lambda (v)
                            (cond
                              [(extension-rename-transformer? v)
                               ;; note that a chain of extension rename transformers is possible,
                               ;; so that's why we have this check inside `syntax-local-value*`
                               ;; instead of outside
                               (free-identifier=? prefix (extension-rename-transformer-extends-id v))]
                              [(portal-syntax? v)
                               (define extends (portal-syntax->extends (portal-syntax-content v)))
                               (and (identifier? extends)
                                    (free-identifier=? prefix extends))]
                              [else #f]))))

(define-for-syntax (identifier-extension-binding-tail-name id)
  (define-values (v next-id) (syntax-local-value/immediate id (lambda () (values #f #f))))
  (cond
    [(extension-rename-transformer? v)
     ;; This is a little hacky. Currently, `build-definitions/maybe-extension` take a
     ;; binding name and a namespace name, but not the "tail" name that is written
     ;; after the last dot. We know that the name for the binding is constructed by
     ;; adding a `.` to the the namespace name, so we recover the "tail" name by string
     ;; operations. We don't recur to handle chains of rename transformer, because we're
     ;; only using this operation for `export` prefixing a definition, and we're interested
     ;; in the immediate definition.
     (define prefix (string-append
                     (symbol->string (syntax-e (extension-rename-transformer-extends-id v)))
                     "."))
     (define full (symbol->string (syntax-e id)))
     (cond
       [(and ((string-length full) . > . (string-length prefix))
             (string=? prefix (substring full 0 (string-length prefix))))
        (string->symbol (substring full (string-length prefix)))]
       [else #f])]
    [else #f]))
