#lang racket/base
(require syntax/parse/pre
         "pack.rkt"
         "annotation-failure.rkt"
         "syntax-wrap.rkt")

(provide extract-ctx
         extract-group-ctx
         extract-ephemeral-ctx)

(define (extract-ctx who ctx-stx
                     #:false-ok? [false-ok? #t]
                     #:report-container? [report-container? #f]
                     #:update [update #f]
                     #:update-tag [update-tag (lambda (tag-stx innner-stx) tag-stx)]
                     #:update-outer [update-outer (lambda (outer-stx innner-stx) outer-stx)]
                     #:annot [annot #f])
  (and (or (not false-ok?) ctx-stx)
       (let ([t (and (syntax*? ctx-stx)
                     (unpack-term ctx-stx #f #f))])
         (unless t
           (raise-annotation-failure who ctx-stx (or annot (if false-ok? "maybe(Term)" "Term"))))
         (define (result v container-mode)
           (if report-container?
               (values v container-mode)
               v))
         (syntax-parse t
           #:datum-literals (op parsed)
           [((~and tag op) id)
            (if update
                (let ([inner-stx (update #'id 'term)])
                  (update-outer (datum->syntax t (list (update-tag #'tag inner-stx) inner-stx) t t)
                                inner-stx))
                (result #'id 'term))]
           [((~and tag parsed) space o)
            (if update
                (let ([inner-stx (update #'o 's-exp)])
                  (update-outer (datum->syntax t (list #'tag #'space inner-stx) t t)
                                inner-stx))
                (result #'o 's-exp))]
           [(head . tail)
            (if update
                (let ([inner-stx (update #'head 'container)])
                  (update-outer (datum->syntax t (cons inner-stx #'tail) t t)
                                inner-stx))
                (result #'head 'container))]
           [_ (if update
                  (update t 'term)
                  (result t 'term))]))))

(define (extract-group-ctx who ctx-stx
                           #:false-ok? [false-ok? #t]
                           #:report-container? [report-container? #f]
                           #:update [update #f]
                           #:update-tag [update-tag (lambda (tag-stx innner-stx) tag-stx)]
                           #:update-outer [update-outer (lambda (outer-stx innner-stx) outer-stx)]
                           #:annot [annot #f])
  (and (or (not false-ok?) ctx-stx)
       (let ([t (and (syntax*? ctx-stx)
                     (unpack-group ctx-stx #f #f))])
         (unless t
           (raise-annotation-failure who ctx-stx (or annot (if false-ok? "maybe(Group)" "Group"))))
         (syntax-parse t
           #:datum-literals (group)
           [((~and tag group) . tail)
            (cond
              [update
               (datum->syntax t (cons (update #'tag 'container) #'tail) t t)]
              [report-container?
               (values #'tag 'container)]
              [else
               #'tag])]
           [_ (error "not a group")]))))

(define (extract-ephemeral-ctx who stx #:update update)
  (let ([t (and (syntax*? stx)
                (unpack-term stx #f #f))])
    (syntax-parse t
      #:datum-literals (parsed)
      [((~and tag parsed) space o)
       (let ([inner-stx (update #'o 's-exp)])
         (update (datum->syntax t (list #'tag #'space inner-stx) t t)
                 's-exp))]
      [else
       (update stx 's-exp)])))
