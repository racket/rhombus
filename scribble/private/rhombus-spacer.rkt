#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "typeset_meta.rhm"
                     "property.rkt"
                     rhombus/private/pack)
         (only-in rhombus
                  [= rhombus-=]
                  [values rhombus-values]))

(provide (for-space rhombus/scribble/typeset
                    ::
                    -:
                    |'|
                    fun
                    val
                    def
                    match
                    for))

(define-syntax (define-spacer stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in_space #'id)
         rhs)]))

(define-for-syntax (spacer proc)
  (Spacer
   (lambda (head tail escape)
     (define-values (new-head new-unpacked-tail)
       (proc head (unpack-tail tail #f #f) escape))
     (values new-head
             (pack-tail new-unpacked-tail)))))

(define-for-syntax (escape? form escape)
  (if (identifier? escape)
      (and (identifier? form)
           (free-identifier=? form escape))
      (syntax-parse form
        #:datum-literals (op)
        [(op id) (free-identifier=? #'id
                                    (cadr (syntax->list escape)))]
        [_ #f])))

(define-for-syntax annote-spacer
  (spacer
   (lambda (head tail escape)
     (values head
             (syntax-parse tail
               #:datum-literals (group)
               [(a . more)
                #:when (not (escape? #'a escape))
                #`(#,(term-identifiers-syntax-property #'a 'typeset-space-name 'ann)
                   . more)]
               [_ tail])))))

(define-spacer :: annote-spacer)
(define-spacer -: annote-spacer)

(define-spacer |'|
  (spacer
   (lambda (head tail escape)
     (values head
             (syntax-parse tail
               #:datum-literals (parens)
               [(((~and tag parens) g) . more)
                #`((tag
                    #,(group-identifiers-syntax-property #'g
                                                         (lambda (id)
                                                           (if (eq? (syntax-e id) '$)
                                                               id
                                                               (syntax-property id 'typeset-space-name #f)))
                                                         #f))
                   . more)]
               [_ tail])))))

(define-spacer fun
  (spacer
   (lambda (head tail escape)
     (fun-spacer head tail escape))))

(define-spacer val
  (spacer
   (lambda (head tail escape)
     (val-spacer head tail escape))))

(define-for-syntax (fun-spacer head tail escape)
  (define (arg-spacer stx)
    (syntax-parse stx
      #:datum-literals (group op)
      #:literals (rhombus-=)
      [((~and tag group) a ... (~and eq (op rhombus-=)) e ...)
       #`(tag #,@(for/list ([a (in-list (syntax->list #'(a ...)))])
                   (term-identifiers-syntax-property a 'typeset-space-name 'bind))
              eq
              e ...)]
      [_ (group-identifiers-syntax-property stx 'typeset-space-name 'bind)]))
  (define (post-spacer stx)
    (syntax-parse stx
      #:datum-literals (op)
      [((~and ann (op _)) . more)
       (cons (term-identifiers-syntax-property #'ann 'typeset-space-name 'bind)
             #'more)]
      [else stx]))
  (define new-tail (syntax-parse tail
                     #:datum-literals (parens group op)
                     [(esc form ((~and tag parens) arg ...) . more)
                      #:when (escape? #'esc escape)
                      #`(esc form
                             #,(cons #'tag (map arg-spacer (syntax->list #'(arg ...))))
                             . #,(post-spacer #'more))]
                     [(id ((~and tag parens) arg ...) . more)
                      #`(id
                         #,(cons #'tag (map arg-spacer (syntax->list #'(arg ...))))
                         . #,(post-spacer #'more))]
                     [(((~and tag parens) arg ...) . more)
                      #`(#,(cons #'tag (map arg-spacer (syntax->list #'(arg ...))))
                         . #,(post-spacer #'more))]
                     [_ tail]))
  (values head new-tail))

(define-for-syntax (val-spacer head tail escape)
  (define new-tail (syntax-parse tail
                     #:datum-literals (group block)
                     [(b ... (~and bl (block . _)))
                      #`(#,@(for/list ([b (in-list (syntax->list #'(b ...)))])
                              (term-identifiers-syntax-property b 'typeset-space-name 'bind))
                         bl)]
                     [_ tail]))
  (values head new-tail))

(define-spacer def
  (spacer
   (lambda (head tail escape)
     (syntax-parse tail
       #:datum-literals (group op)
       [(esc form ((~and tag parens) arg ...) . more)
        #:when (escape? #'esc escape)
        (fun-spacer head tail escape)]
       [(id ((~and tag parens) arg ...) . more)
        #:when (and (identifier? #'id)
                    (not (free-identifier=? #'id #'rhombus-values)))
        (fun-spacer head tail escape)]
       [else
        (val-spacer head tail escape)]))))

(define-spacer match
  (spacer
   (lambda (head tail escape)
     (define (binding-spacer stx)
       (term-identifiers-syntax-property stx 'typeset-space-name 'bind))
     (define new-tail (syntax-parse tail
                        #:datum-literals (alts group)
                        [(expr ... ((~and tag alts) b ...))
                         #`(expr ...
                                 (tag
                                  #,@(for/list ([b (in-list (syntax->list #'(b ...)))])
                                       (syntax-parse b
                                         #:datum-literals (block)
                                         [((~and tag block) ((~and g-tag group) form ... (~and b (block . _))))
                                          #`(tag
                                             (g-tag #,@(map binding-spacer (syntax->list #'(form ...)))
                                                    b))]
                                         [_ b]))))]
                        [_ tail]))
     (values head new-tail))))

(define-spacer for
  (spacer
   (lambda (head tail escape)
     (define new-tail (syntax-parse tail
                        #:datum-literals (group block)
                        [(((~and block-tag block) body ... ((~and group-tag group) (~and into #:into) reducer ...)))
                         #`((block-tag
                             #,@(map for-body-spacer (syntax->list #'(body ...)))
                             (group-tag into #,(for/list ([reducer (in-list (syntax->list #'(reducer ...)))])
                                                 (term-identifiers-syntax-property reducer 'typeset-space-name 'reducer)))))]
                        [(reducer ... ((~and block-tag block) body ...))
                         #`(#,@(for/list ([reducer (in-list (syntax->list #'(reducer ...)))])
                                 (term-identifiers-syntax-property reducer 'typeset-space-name 'reducer))
                            (block-tag
                             #,@(map for-body-spacer (syntax->list #'(body ...)))))]
                        [_
                         tail]))
     (values head new-tail))))

(define-for-syntax (for-body-spacer body)
  (syntax-parse body
    [(group #:do id . args)
     #`(group #:do #,(term-identifiers-syntax-property #'id 'typeset-space-name 'for_clause) . args)]
    [_
     body]))
