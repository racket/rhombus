#lang racket/base
(require shrubbery/property
         syntax/parse/pre
         racket/syntax-srcloc)

(provide relocate-span
         relocate*
         relevant-source-syntax)

(define (relocate-span stx ctx-stxes-in)
  (define keep-raw-interior? #f) ; expose this as an option?
  (define ctx-stxes (map relevant-source-syntax ctx-stxes-in))
  (define (combine-raw a b) (if (null? a) b (if (null? b) a (cons a b))))
  (let loop ([ctx-stxes (cdr ctx-stxes)]
             [loc (syntax-srcloc (car ctx-stxes))]
             [pre (or (syntax-raw-prefix-property (car ctx-stxes)) null)]
             [raw (if keep-raw-interior?
                      (or (syntax-raw-property (car ctx-stxes)) null)
                      null)]
             [suffix (combine-raw
                      (if keep-raw-interior?
                          (or (syntax-raw-tail-property (car ctx-stxes)) null)
                          null)
                      (if (or keep-raw-interior?
                              (null? (cdr ctx-stxes)))
                          (or (syntax-raw-suffix-property (car ctx-stxes)) null)
                          null))])
    (cond
      [(null? ctx-stxes)
       (let* ([ctx (datum->syntax #f #f loc)]
              [ctx (if (null? pre)
                       ctx
                       (syntax-raw-prefix-property ctx pre))]
              [ctx (syntax-raw-property ctx raw)]
              [ctx (if (null? suffix)
                       ctx
                       (syntax-raw-suffix-property ctx suffix))])
         (relocate* stx ctx))]
      [(and (pair? (cdr ctx-stxes))
            (not keep-raw-interior?))
       (loop (cdr ctx-stxes) loc pre raw suffix)]
      [else
       (define empty-raw? (and (null? raw) (null? suffix)))
       (define ctx (car ctx-stxes))
       (define new-raw (or (syntax-raw-property ctx) null))
       (define new-loc (syntax-srcloc ctx))
       (loop (cdr ctx-stxes)
             (if (and loc
                      new-loc
                      (equal? (srcloc-source loc)
                              (srcloc-source new-loc)))
                 (srcloc (srcloc-source loc)
                         (srcloc-line loc)
                         (srcloc-column loc)
                         (srcloc-position loc)
                         (if (and (srcloc-position new-loc)
                                  (srcloc-span new-loc)
                                  (srcloc-position loc))
                             (max (- (+ (srcloc-position new-loc)
                                        (srcloc-span new-loc))
                                     (srcloc-position loc))
                                  0)
                             (srcloc-span loc)))
                 loc)
             (if empty-raw?
                 (combine-raw pre (or (syntax-raw-prefix-property ctx) null))
                 pre)
             (if empty-raw?
                 (or (syntax-raw-property ctx) null)
                 (combine-raw (combine-raw (combine-raw raw suffix)
                                           (or (syntax-raw-prefix-property ctx) null))
                              (or (syntax-raw-property ctx) null)))
             (combine-raw (if keep-raw-interior?
                              (or (syntax-raw-tail-property ctx) null)
                              null)
                          (combine-raw
                           (or (syntax-raw-tail-suffix-property ctx) null)
                           (or (syntax-raw-suffix-property ctx) null))))])))

(define (relocate* stx ctx-stx-in)
  (define ctx-stx (relevant-source-syntax ctx-stx-in))
  #;(log-error "?? ~s" (syntax->datum stx))
  #;(log-error " : ~s" (syntax->datum ctx-stx-in))
  #;(log-error " = ~s" (syntax->datum ctx-stx))
  (define (relocate stx)
    #;(log-error " ! ~s" (syntax->datum stx))
    (datum->syntax stx (syntax-e stx) ctx-stx ctx-stx))
  (let loop ([stx stx])
    (syntax-parse stx
      #:datum-literals (group block alts parens brackets braces quotes multi op)
      [((~and head (~or group block alts parens brackets braces quotes)) . rest)
       (datum->syntax #f (cons (relocate #'head) #'rest))]
      [((~and m multi) (g t))
       #:when (syntax-property #'g 'from-pack)
       (loop #'t)]
      [((~and m multi) (g . rest))
       (datum->syntax #f (list #'m (cons (relocate #'g) #'rest)))]
      [((~and tag op) o)
       (datum->syntax #f (list #'tag (relocate #'o)))]
      [_
       (relocate stx)])))

(define (relevant-source-syntax ctx-stx-in)
  (syntax-parse ctx-stx-in
    #:datum-literals (group block alts parens brackets braces quotes multi op)
    [((~and head (~or group block alts parens brackets braces quotes)) . _) #'head]
    [(multi (g t))
     #:when (syntax-property #'g 'from-pack)
     (relevant-source-syntax #'t)]
    [(multi (g . _)) #'g]
    [(op o) #'o]
    [_ ctx-stx-in]))
