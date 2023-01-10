#lang racket/base
(require syntax/parse/pre)

(provide term-identifiers-syntax-property
         group-identifiers-syntax-property)

(define (term-identifiers-syntax-property stx property val [preserved? #t])
  (define (add-to-atom a)
    (if (procedure? property)
        (property a)
        (syntax-property a property val preserved?)))
  (define (add-to-groups gs)
    (for/list ([g (in-list (syntax->list gs))])
      (add-to-group g)))
  (define (add-to-group g)
    (syntax-parse g
      #:datum-literals (group)
      [((~and tag group) t ...)
       (datum->syntax g (cons #'tag (add-to-terms #'(t ...))) g g)]))
  (define (add-to-terms ts)
    (for/list ([t (in-list (syntax->list ts))])
      (add-to-term t)))
  (define (add-to-term stx)
    (syntax-parse stx
      #:datum-literals (parens brackets braces block alts op)
      [((~and tag (~or parens brackets braces block)) g ...)
       (datum->syntax stx (cons #'tag (add-to-groups #'(g ...))) stx stx)]
      [((~and tag alts) b ...)
       (datum->syntax stx (cons #'tag (add-to-terms #'(b ...))) stx stx)]
      [((~and tag parens) g ...)
       (datum->syntax stx (cons #'tag (add-to-groups #'(g ...))) stx stx)]
      [((~and tag parens) g ...)
       (datum->syntax stx (cons #'tag (add-to-groups #'(g ...))) stx stx)]
      [((~and tag op) id)
       (datum->syntax stx (list #'tag (add-to-atom #'id)) stx stx)]
      [_ (add-to-atom stx)]))
  (add-to-term stx))

(define (group-identifiers-syntax-property g-stx property val [preserved? #t])
  (syntax-parse g-stx
    #:datum-literals (group)
    [((~and tag group) t ...)
     (datum->syntax g-stx
                    (cons #'tag
                          (for/list ([t (in-list (syntax->list #'(t ...)))])
                            (term-identifiers-syntax-property t property val preserved?)))
                    g-stx
                    g-stx)]))
