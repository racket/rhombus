#lang racket/base
(require syntax/parse/pre
         "pack.rkt"
         "realm.rkt")

(provide extract-ctx
         extract-group-ctx)

(define (extract-ctx who ctx-stx
                     #:false-ok? [false-ok? #t]
                     #:report-container? [report-container? #f]
                     #:update [update #f]
                     #:update-tag [update-tag (lambda (tag-stx innner-stx) tag-stx)]
                     #:update-outer [update-outer (lambda (outer-stx innner-stx) outer-stx)]
                     #:annot [annot #f])
  (and (or (not false-ok?) ctx-stx)
       (let ([t (and (syntax? ctx-stx)
                     (unpack-term ctx-stx #f #f))])
         (unless t
           (raise-argument-error* who rhombus-realm (or annot (if false-ok? "maybe(Term)" "Term")) ctx-stx))
         (define (result v container?)
           (if report-container?
               (values v container?)
               v))
         (syntax-parse t
           #:datum-literals (op parsed)
           [((~and tag op) id)
            (if update
                (let ([inner-stx (update #'id #f)])
                  (update-outer (datum->syntax t (list (update-tag #'tag inner-stx) inner-stx) t t)
                                inner-stx))
                (result #'id #f))]
           [((~and tag parsed) space o)
            (if update
                (let ([inner-stx (update #'o #f)])
                  (update-outer (datum->syntax t (list #'tag #'space inner-stx) t t)
                                inner-stx))
                (result #'o #f))]
           [(head . tail)
            (if update
                (let ([inner-stx (update #'head #t)])
                  (update-outer (datum->syntax t (cons inner-stx #'tail) t t)
                                inner-stx))
                (result #'head #t))]
           [_ (if update
                  (update t #f)
                  (result t #f))]))))

(define (extract-group-ctx who ctx-stx
                           #:false-ok? [false-ok? #t]
                           #:report-container? [report-container? #f]
                           #:update [update #f]
                           #:update-tag [update-tag (lambda (tag-stx innner-stx) tag-stx)]
                           #:update-outer [update-outer (lambda (outer-stx innner-stx) outer-stx)]
                           #:annot [annot #f])
  (and (or (not false-ok?) ctx-stx)
       (let ([t (and (syntax? ctx-stx)
                     (unpack-group ctx-stx #f #f))])
         (unless t
           (raise-argument-error* who rhombus-realm (or annot (if false-ok? "maybe(Group)" "Group")) ctx-stx))
         (syntax-parse t
           #:datum-literals (group)
           [((~and tag group) . tail)
            (cond
              [update
               (datum->syntax t (cons (update #'tag #t) #'tail) t t)]
              [report-container?
               (values #'tag #t)]
              [else
               #'tag])]
           [_ (error "not a group")]))))
