#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "typeset_meta.rhm"
                     "property.rkt"))

(provide (for-space rhombus/scribble/typeset
                    ::
                    |'|
                    fun
                    match))

(define-syntax (define-spacer stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in_space #'id)
         rhs)]))

(define-for-syntax (escape? form escape)
  (if (identifier? escape)
      (and (identifier? form)
           (free-identifier=? form escape))
      (syntax-parse form
        #:datum-literals (op)
        [(op id) (free-identifier=? #'id
                                    (cadr (syntax->list escape)))]
        [_ #f])))

(define-spacer ::
  (Spacer
   (lambda (head tail escape)
     (values head
             (syntax-parse tail
               #:datum-literals (parens group)
               [(parens (group a . more))
                #:when (not (escape? #'a escape))
                #`(parens
                   (group
                    #,(term-identifiers-syntax-property #'a 'typeset-space-name 'ann)
                    . more))]
               [_ tail])))))

(define-spacer |'|
  (Spacer
   (lambda (head tail escape)
     (values head
             (syntax-parse tail
               #:datum-literals (parens group)
               [(parens (group ((~and tag parens) g) . more))
                #`(parens
                   (group
                    (tag
                     #,(group-identifiers-syntax-property #'g
                                                          (lambda (id)
                                                            (if (eq? (syntax-e id) '$)
                                                                id
                                                                (syntax-property id 'typeset-space-name #f)))
                                                          #f))
                    . more))]
               [_ tail])))))

(define-spacer fun
  (Spacer
   (lambda (head tail escape)
     (define (arg-spacer stx)
       (group-identifiers-syntax-property stx 'typeset-space-name 'bind))
     (define (post-spacer stx)
       (syntax-parse stx
         #:datum-literals (op)
         [((~and ann (op _)) . more)
          (cons (term-identifiers-syntax-property #'ann 'typeset-space-name 'bind)
                #'more)]
         [else stx]))
     (define new-tail (syntax-parse tail
                        #:datum-literals (parens group op)
                        [(parens (group esc form ((~and tag parens) arg ...) . more))
                         #:when (escape? #'esc escape)
                         #`(parens
                            (group
                             esc form
                             #,(cons #'tag (map arg-spacer (syntax->list #'(arg ...))))
                             . #,(post-spacer #'more)))]
                        [(parens (group id ((~and tag parens) arg ...) . more))
                         #`(parens
                            (group
                             id
                             #,(cons #'tag (map arg-spacer (syntax->list #'(arg ...))))
                             . #,(post-spacer #'more)))]
                        [(parens (group ((~and tag parens) arg ...) . more))
                         #`(parens
                            (group
                             #,(cons #'tag (map arg-spacer (syntax->list #'(arg ...))))
                             . #,(post-spacer #'more)))]
                        [_ tail]))
     (values head new-tail))))

(define-spacer match
  (Spacer
   (lambda (head tail escape)
     (define (binding-spacer stx)
       (term-identifiers-syntax-property stx 'typeset-space-name 'bind))
     (define new-tail (syntax-parse tail
                        #:datum-literals (alts group)
                        [(parens (group expr ... ((~and tag alts) b ...)))
                         #`(parens
                            (group
                             expr ...
                             (tag
                              #,@(for/list ([b (in-list (syntax->list #'(b ...)))])
                                   (syntax-parse b
                                     #:datum-literals (block)
                                     [((~and tag block) ((~and g-tag group) form ... (~and b (block . _))))
                                      #`(tag
                                         (g-tag #,@(map binding-spacer (syntax->list #'(form ...)))
                                                b))]
                                     [_ b])))))]
                        [_ tail]))
     (values head new-tail))))
