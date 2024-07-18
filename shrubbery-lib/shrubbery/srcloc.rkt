#lang racket/base
(require (for-syntax racket/base))

(provide syntax-srcloc)

(define-syntax (get stx)
  (syntax-case stx ()
    [(_ syntax-srcloc)
     (cond
       [(identifier-binding #'syntax-srcloc)
        #'(begin)]
       [(file-exists? (collection-file-path "syntax-srcloc.rkt" "racket"))
        #`(require #,(datum->syntax #'syntax-srcloc 'racket/syntax-srcloc))]
       [else
        #'(define (syntax-srcloc stx)
            (srcloc (syntax-source stx)
                    (syntax-line stx)
                    (syntax-column stx)
                    (syntax-position stx)
                    (syntax-span stx)))])]))

(get syntax-srcloc)
