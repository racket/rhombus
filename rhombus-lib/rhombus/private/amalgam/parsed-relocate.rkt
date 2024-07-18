#lang racket/base
(require syntax/parse/pre)
         
(provide parsed-relocate*)

;; needed for `runtime_path.def`

(define (parsed-relocate* at-stx stx)
  (syntax-parse stx
    #:datum-literals (multi group)
    [(multi (group e))
     #`(multi (group #,(parsed-relocate* at-stx #'e)))]
    [(parsed-tag space e)
     (datum->syntax at-stx
                    (list #'parsed-tag #'space
                          (datum->syntax at-stx
                                         (syntax-e #'e)
                                         at-stx
                                         at-stx))
                    at-stx
                    at-stx)]))
