#lang racket/base
(require syntax/parse/pre)

(provide track-parsed-expression
         track-parsed-sequence-origin)

(define (track-parsed-expression stx from-stx id)
  (syntax-parse stx
    #:datum-literals (parsed)
    [((~and tag parsed) kw e)
     (datum->syntax
      stx
      (list #'tag #'kw (syntax-track-origin #'e
                                            from-stx
                                            id))
      stx
      stx)]
    [_ (syntax-track-origin stx from-stx id)]))

(define (track-parsed-sequence-origin stxes from-stx id)
  (datum->syntax
   #f
   (for/list ([stx (in-list (syntax->list stxes))])
     (syntax-parse stx
       #:datum-literals (group parsed)
       [((~and g-tag group) (~and inner ((~and tag parsed) kw (qs e local-kw))))
        (datum->syntax stx
                       (list #'g-tag
                             (datum->syntax
                              #'inner
                              (list #'tag #'kw (list #'qs
                                                     (syntax-track-origin #'e
                                                                          from-stx
                                                                          id)
                                                     #'local-kw))
                              #'inner
                              #'inner))
                       stx
                       stx)]
       [_ stx]))))
