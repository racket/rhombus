#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     (prefix-in typeset-meta: "typeset_meta.rhm")
                     shrubbery/property)
         racket/list
         (only-in rhombus
                  def val fun :: |'| |.|)
         (only-in rhombus/macro
                  defn)
         (only-in "rhombus.rhm"
                  rhombusblock)
         (only-in rhombus/parse
                  rhombus-expression)
         (only-in scribble/manual
                  hspace
                  racketvarfont)
         (only-in scribble/racket
                  symbol-color
                  syntax-def-color
                  value-def-color)
         (only-in scribble/core
                  table
                  paragraph
                  element
                  plain)
         (only-in scribble/private/manual-vars
                  boxed-style))

(provide typeset-doc)

(define-syntax (typeset-doc stx)
  (syntax-parse stx
    #:datum-literals (parens group brackets)
    [(_ (parens (group form ...) ...
                (group
                 (brackets content-group ...))))
     (define forms (syntax->list #'((group form ...) ...)))
     (define def-ids (for/list ([form (in-list forms)])
                       (extract-defined form)))
     (define wrap-defs (for/fold ([rev-wrap-defs '()] [seen #hasheq()] #:result (reverse rev-wrap-defs))
                                 ([def-id (in-list def-ids)])
                         (cond
                           [(hash-ref seen (syntax-e def-id) #f)
                            (values (cons #'(values) rev-wrap-defs)
                                    seen)]
                           [else
                            (values (cons #`(let-syntax ([#,(typeset-meta:in_space def-id)
                                                          (make-def-id-transformer (quote-syntax #,def-id))]))
                                          rev-wrap-defs)
                                    (hash-set seen (syntax-e def-id) #t))])))
     (define vars (for/fold ([vars #hasheq()]) ([form (in-list forms)])
                    (extract-metavariables form vars)))
     (define t-forms (map extract-typeset forms))
     (with-syntax ([(t-form ...) t-forms]
                   [(t-block ...) (for/list ([t-form (in-list t-forms)])
                                    (syntax-raw-property
                                     (datum->syntax #f 'block
                                                    (syntax-parse t-form
                                                      [(_ a . _) #'a]))
                                     ""))]
                   [((wrap-def ...) ...) wrap-defs])
       #`(let-syntax #,(for/list ([id (in-hash-values vars)])
                         #`[#,(typeset-meta:in_space id) (make-meta-id-transformer (quote-syntax #,id))])
           (list
            (table
             boxed-style
             (insert-blank-lines
              (list
               (list (wrap-def ... (rhombus-expression (group rhombusblock (t-block t-form)))))
               ...)))
            (rhombus-expression content-group)
            ...)))]))

(define-for-syntax (make-def-id-transformer id)
  (typeset-meta:Transformer
   (lambda (use-stx)
     #`(parsed (element symbol-color (element value-def-color #,(symbol->string (syntax-e id))))))))

(define-for-syntax (make-meta-id-transformer id)
  (typeset-meta:Transformer
   (lambda (use-stx)
     #`(parsed (racketvarfont #,(symbol->string (syntax-e id)))))))

(define-for-syntax (extract-defined stx)
  (syntax-parse stx
    #:literals (def val fun :: defn |.| |'|)
    #:datum-literals (parens group op)
    [(group (~or def fun) id:identifier (parens g ...) . _) #'id]
    [(group (~or def val) id:identifier . _) #'id]
    [(group defn (op |.|) macro (op |'|) (parens (group id t ...))) #'id]
    [_ (raise-syntax-error 'doc "unknown definition form" stx)]))

(define-for-syntax (add-metavariable vars id)
  (hash-set vars (syntax-e id) (or (hash-ref vars (syntax-e id) #f) id)))

(define-for-syntax (extract-metavariables stx vars)
  (syntax-parse stx
    #:literals (def val fun :: defn |.| |'|)
    #:datum-literals (parens group op)
    [(group (~or def fun) id:identifier (parens g ...) . _)
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-binding-metavariables g vars))]
    [(group (~or def val) id:identifier . _) vars]
    [(group defn (op |.|) macro (op |'|) (parens (group id t ...)))
     (extract-group-metavariables #'(group t ...)  vars)]
    [_ vars]))

(define-for-syntax (extract-binding-metavariables stx vars)
  (syntax-parse stx
    #:literals (def val fun ::)
    #:datum-literals (parens group op)
    [(group id (op ::) _) (add-metavariable vars #'id)]
    [_ vars]))

(define-for-syntax (extract-group-metavariables g vars)
  (syntax-parse g
    #:datum-literals (group)
    [(group t ...)
     (for/fold ([vars vars]) ([t (in-list (syntax->list #'(t ...)))])
       (extract-term-metavariables t vars))]))

(define-for-syntax (extract-term-metavariables t vars)
  (syntax-parse t
    [(tag g ...)
     #:when (memq (syntax-e #'tag) '(parens brackets braces block))
     (for/fold ([vars vars]) ([g (in-list (syntax->list #'(g ...)))])
       (extract-group-metavariables g vars))]
    [id:identifier (add-metavariable vars #'id)]
    [_ vars]))

(define-for-syntax (extract-typeset stx)
  (syntax-parse stx
    #:literals (defn |.| |'|)
    #:datum-literals (parens group op macro)
    [(group defn (op |.|) macro (op |'|) (parens g)) #'g]
    [_ stx]))

(define (insert-blank-lines l)
  (add-between l (list (paragraph plain (hspace 1)))))
