#lang racket/base
(require syntax/parse/pre)

(provide track-parsed-sequence-origin)

(define (track-parsed-sequence-origin stxes from-stx id)
  (datum->syntax
   #f
   (for/list ([stx (in-list (syntax->list stxes))])
     (syntax-parse stx
       #:datum-literals (group parsed)
       [((~and g-tag group) (~and inner ((~and tag parsed) kw e)))
        (datum->syntax stx
                       (list #'g-tag
                             (datum->syntax
                              #'inner
                              (list #'tag #'kw (syntax-track-origin #'e
                                                                    from-stx
                                                                    id))
                              #'inner
                              #'inner))
                       stx
                       stx)]
       [_ stx]))))
