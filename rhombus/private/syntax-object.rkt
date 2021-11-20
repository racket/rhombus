#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse
         racket/syntax-srcloc
         "expression.rkt")

(provide literal_syntax
         literal_syntax_term
         to_syntax
         relocate_syntax
         relocate_span_syntax)

(define-syntax literal_syntax
  (expression-transformer
   #'literal_syntax
   (lambda (stx)
     (syntax-parse stx
       [(_ form . tail)
        (values #'(quote-syntax form) #'tail)]))))

(define-syntax literal_syntax_term
  (expression-transformer
   #'literal_syntax
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group)
       [(_ (parens (group term)) . tail)
        (values #'(quote-syntax term) #'tail)]))))

;; ----------------------------------------

(define (relevant-source-syntax ctx-stx-in)
  (syntax-parse ctx-stx-in
    #:datum-literals (group block alts parens brackets braces op)
    [((~and head (~or group block alts parens brackets braces)) . _) #'head]
    [(op o) #'o]
    [_ ctx-stx-in]))

(define (to_syntax v)
  (datum->syntax #f v))

(define (relocate_syntax stx ctx-stx-in)
  (unless (syntax? stx) (raise-argument-error 'relocate_syntax "syntax?" stx))
  (define ctx-stx (relevant-source-syntax ctx-stx-in))
  (syntax-parse stx
    #:datum-literals (group block alts parens brackets braces op)
    [((~and head (~or group block alts parens brackets braces)) . rest)
     #`(#,(datum->syntax #'head (syntax-e #'head) ctx-stx ctx-stx) . rest)]
    [(op o)
     #`(op #,(datum->syntax #'o (syntax-e #'o) ctx-stx ctx-stx) . rest)]
    [_
     (datum->syntax stx (syntax-e stx) ctx-stx ctx-stx)]))

(define (relocate_span_syntax stx ctx-stxes-in)
  (unless (syntax? stx) (raise-argument-error 'relocate_span_syntax "syntax?" stx))
  (define ctx-stxes (map relevant-source-syntax ctx-stxes-in))
  (define (combine-raw a b) (if (null? a) b (if (null? b) a (cons a b))))
  (let loop ([ctx-stxes (cdr ctx-stxes)]
             [loc (syntax-srcloc (car ctx-stxes))]
             [pre (or (syntax-property (car ctx-stxes) 'raw-prefix) null)]
             [raw (or (syntax-property (car ctx-stxes) 'raw) null)]
             [suffix (combine-raw (or (syntax-property (car ctx-stxes) 'raw-suffix) null)
                                  ;; treating tail like suffix on te grounds that
                                  ;; we do not expect `stx` to be a head like `parens`
                                  (or (syntax-property (car ctx-stxes) 'raw-tail) null))])
    (cond
      [(null? ctx-stxes)
       (let* ([ctx (datum->syntax #f #f loc)]
              [ctx (if (null? pre)
                       ctx
                       (syntax-property ctx 'raw-prefix pre))]
              [ctx (syntax-property ctx 'raw raw)]
              [ctx (if (null? suffix)
                       ctx
                       (syntax-property ctx 'raw-suffix suffix))])
         (relocate_syntax stx ctx))]
      [else
       (define empty-raw? (and (null? raw) (null? suffix)))
       (define ctx (car ctx-stxes))
       (define new-raw (or (syntax-property ctx 'raw) null))
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
                             (- (+ (srcloc-position new-loc)
                                   (srcloc-span new-loc))
                                (srcloc-position loc))
                             (srcloc-span loc)))
                 loc)
             (if empty-raw?
                 (combine-raw pre (or (syntax-property ctx 'raw-prefix) null))
                 pre)
             (if empty-raw?
                 (or (syntax-property ctx 'raw) null)
                 (combine-raw (combine-raw (combine-raw raw suffix)
                                           (or (syntax-property ctx 'raw-prefix) null))
                              (or (syntax-property ctx 'raw) null)))
             (combine-raw (or (syntax-property ctx 'raw-suffix) null)
                          (or (syntax-property ctx 'raw-tail) null)))])))

