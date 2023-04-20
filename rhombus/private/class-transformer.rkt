#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt"
                     "with-syntax.rkt"
                     "srcloc.rkt")
         "parse.rkt"
         "entry-point.rkt"
         "pack.rkt"
         "parens.rkt")

(provide wrap-class-transformer
         (for-syntax check-class-transformer))

(define-syntax (wrap-class-transformer stx)
  (check-class-transformer stx)
  (syntax-parse stx
    #:literals ()
    #:datum-literals (group named-macro)
    [(_ name (named-macro macro orig-stx (q-tag::quotes ((~and g-tag group) id:identifier . pat)) blk) make what)
     (define empty-parens (datum->syntax #'id '(parens) #'id #'id))
     #`(wrap-class-transformer name (#,group-tag macro (q-tag (g-tag #,empty-parens . pat)) blk) make what)]
    [(_ name (named-macro macro orig-stx (a-tag::alts
                                          (b-tag::block
                                           ((~and outer-g-tag group)
                                            (q-tag::quotes ((~and g-tag group) id:identifier . pat))
                                            blk))
                                          ...))
        make
        what)
     (with-syntax ([(empty-parens ...) (for/list ([id (in-list (syntax->list #'(id ...)))])
                                         (datum->syntax id '(parens) id id))]
                   [g-tag group-tag])
       #'(wrap-class-transformer name (g-tag
                                       macro
                                       (a-tag (b-tag (outer-g-tag (q-tag (g-tag empty-parens . pat)) blk))
                                              ...))
                                 make
                                 what))]
    [(_ name g make-prefix-operator what)
     (with-syntax-parse ([(~var lam (:entry-point no-adjustments)) (respan #'g)])
       #'(make-prefix-operator #'name
                               '((default . stronger))
                               'macro
                               (let ([name lam.parsed])
                                 (let ([name (lambda (tail head)
                                               (name (pack-tail
                                                      (cons head (unpack-tail tail #f #f)))))])
                                   name))))]))

(define-for-syntax (check-class-transformer stx)
  (syntax-parse stx
    #:literals ()
    #:datum-literals (group named-macro)
    [(_ name (named-macro macro orig-stx (q-tag::quotes ((~and g-tag group) id:identifier . pat)) blk) what . _)
     (unless (eq? (syntax-e #'name) (syntax-e #'id))
       (raise-syntax-error #f
                           (format "name in pattern does not match the ~a name" (syntax-e #'what))
                           #'orig-stx
                           #'id))]
    [(_ name (named-macro macro orig-stx (a-tag::alts
                                         (b-tag::block
                                          ((~and outer-g-tag group)
                                           (q-tag::quotes ((~and g-tag group) id:identifier . pat))
                                           blk))
                                         ...)) what . _)
     (for ([id (in-list (syntax->list #'(id ...)))])
       (unless (eq? (syntax-e #'name) (syntax-e id))
         (raise-syntax-error #f
                             (format "name in pattern does not match the ~a name" (syntax-e #'what))
                             #'orig-stx
                             id)))]
    [(_ name (named-macro macro orig-stx . _) . _)
     (raise-syntax-error #f "invalid pattern form" #'orig-stx)]
    [(_ name g . _)
     (void)]))
