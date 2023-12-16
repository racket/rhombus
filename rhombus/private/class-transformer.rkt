#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt"
                     "srcloc.rkt"
                     (submod "entry-point-adjustment.rkt" for-struct))
         "entry-point.rkt"
         "pack.rkt"
         "parens.rkt")

(provide wrap-class-transformer
         (for-syntax check-class-transformer))

(define-syntax (wrap-class-transformer stx)
  (check-class-transformer stx)
  (syntax-parse stx
    [(_ name tail-name g make-prefix-operator what)
     #:with (~var lam (:entry-point no-adjustments))
     (syntax-parse #'g
       #:datum-literals (group named-macro)
       [(named-macro macro
                     orig-stx
                     (q-tag::quotes ((~and g-tag group) id:identifier . pat))
                     blk)
        #:with empty-parens (datum->syntax #'id '(parens) #'id #'id)
        (respan #`(#,group-tag macro
                               (q-tag (g-tag empty-parens . pat))
                               blk))]
       [(named-macro macro
                     orig-stx
                     (a-tag::alts
                      (b-tag::block
                       ((~and outer-g-tag group)
                        (q-tag::quotes ((~and g-tag group) id:identifier . pat))
                        blk))
                      ...))
        #:with (empty-parens ...) (for/list ([id (in-list (syntax->list #'(id ...)))])
                                             (datum->syntax id '(parens) id id))
        (respan #`(#,group-tag macro
                               (a-tag
                                (b-tag
                                 (outer-g-tag
                                  (q-tag (g-tag empty-parens . pat)) blk))
                                ...)))]
       [_ #'g])
     #`(make-prefix-operator #'name
                             '((default . stronger))
                             'macro
                             (let ([name lam.parsed])
                               (let ([name (lambda (tail head)
                                             (name (pack-tail
                                                    (cons head (unpack-tail tail #f #f)))))])
                                 name)))]))

(define-for-syntax (check-class-transformer stx)
  (syntax-parse stx
    #:literals ()
    #:datum-literals (group named-macro)
    [(_ name tail-name (named-macro macro orig-stx (q-tag::quotes ((~and g-tag group) id:identifier . pat)) blk) _ what . _)
     (unless (eq? (syntax-e #'tail-name) (syntax-e #'id))
       (raise-syntax-error #f
                           (format "name in pattern does not match the prefixless ~a name" (syntax-e #'what))
                           #'orig-stx
                           #'id))]
    [(_ name tail-name (named-macro macro orig-stx (a-tag::alts
                                                    (b-tag::block
                                                     ((~and outer-g-tag group)
                                                      (q-tag::quotes ((~and g-tag group) id:identifier . pat))
                                                      blk))
                                                    ...)) _ what . _)
     (for ([id (in-list (syntax->list #'(id ...)))])
       (unless (eq? (syntax-e #'tail-name) (syntax-e id))
         (raise-syntax-error #f
                             (format "name in pattern does not match the prefixless ~a name" (syntax-e #'what))
                             #'orig-stx
                             id)))]
    [(_ name tail-name (named-macro macro orig-stx . _) . _)
     (raise-syntax-error #f "invalid pattern form" #'orig-stx)]
    [(_ name tail-name g . _)
     (void)]))
