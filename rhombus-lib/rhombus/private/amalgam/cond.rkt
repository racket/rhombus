#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/list
                     "srcloc.rkt")
         "expression.rkt"
         "repetition.rkt"
         "parse.rkt"
         "else-clause.rkt"
         "static-info.rkt"
         "parens.rkt"
         "srcloc-error.rkt"
         "static-info.rkt"
         "compound-repetition.rkt")

(provide (rename-out [rhombus-if if]
                     [rhombus-cond cond]
                     [rhombus-when when]
                     [rhombus-unless unless])
         (for-space rhombus/repet
                    (rename-out [rhombus-if if])))

(define-syntax rhombus-if
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id test ... (_::alts alt ...))
        (when (null? (syntax-e #'(test ...)))
          (raise-syntax-error #f
                              "missing test expression"
                              stx))
        (syntax-parse #'(alt ...)
          [((tag-thn::block thn ...)
            (tag-els::block els ...))
           (define thn-e (enforest-expression-block #'(tag-thn thn ...)))
           (define els-e (enforest-expression-block #'(tag-els els ...)))
           (values
            (wrap-static-info*
             (relocate+reraw
              (respan stx)
              #`(if (rhombus-expression (group test ...))
                    #,(discard-static-infos thn-e)
                    #,(discard-static-infos els-e)))
             (static-infos-or (extract-static-infos thn-e)
                                 (extract-static-infos els-e)))
            #'())]
          [_
           (raise-syntax-error #f
                               "expected two alternatives"
                               stx)])]))))

(define-repetition-syntax rhombus-if
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id test ... (_::alts alt ...))
        (when (null? (syntax-e #'(test ...)))
          (raise-syntax-error #f
                              "missing test repetition"
                              stx))
        (syntax-parse #'(alt ...)
          [((~and thn (_::block))
            (_::block els ...))
           (raise-syntax-error #f
                               "block has no repetitions"
                               stx
                               #'thn)]
          [((_::block thn ...)
            (~and els (_::block)))
           (raise-syntax-error #f
                               "block has no repetitions"
                               stx
                               #'els)]
          [((tag-thn::block thn ...)
            (tag-els::block els ...))
           #:with tst-r::repetition #'(group test ...)
           #:with (thn-r::repetition ...) #'(thn ...)
           #:with (els-r::repetition ...) #'(els ...)
           (values
            (build-compound-repetition
             stx
             (syntax->list #'(tst-r.parsed thn-r.parsed ... els-r.parsed ...))
             #:element-statinfo? #t
             (lambda tst+thns+elss
               (define n-thns (length (syntax->list #'(thn ...))))
               (define tst (car tst+thns+elss))
               (define thns (take (cdr tst+thns+elss) n-thns))
               (define elss (list-tail (cdr tst+thns+elss) n-thns))
               (values #`(if #,(discard-static-infos tst)
                             (begin . #,(map discard-static-infos thns))
                             (begin . #,(map discard-static-infos elss)))
                       (static-infos-or
                        (extract-static-infos (last thns))
                        (extract-static-infos (last elss))))))
            #'())]
          [_
           (raise-syntax-error #f
                               "expected two alternatives"
                               stx)])]))))

(define-syntax rhombus-cond
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (_::alts
                  (_::block (group pred ... (tag::block rhs ...)))
                  ...
                  e::else-clause))
        (define rhs-es (map enforest-expression-block
                            (syntax->list #'((tag rhs ...) ...))))
        (define els-e (enforest-expression-block #'e.rhs))
        (values
         (with-syntax ([(rhs ...) (map discard-static-infos rhs-es)])
           (wrap-static-info*
            (relocate+reraw
             (respan stx)
             #`(cond
                 [(rhombus-expression (group pred ...)) rhs]
                 ...
                 [else #,(discard-static-infos els-e)]))
            (for/fold ([si (extract-static-infos els-e)]) ([rhs-e (in-list rhs-es)])
              (static-infos-or si (extract-static-infos rhs-e)))))
         #'())]
       [(form-id (_::alts
                  (_::block (group pred ... (tag::block rhs ...)))
                  ...))
        (define rhs-es (map enforest-expression-block
                            (syntax->list #'((tag rhs ...) ...))))
        (values
         (with-syntax ([(rhs ...) (map discard-static-infos rhs-es)])
           (wrap-static-info*
            (relocate+reraw
             (respan stx)
             #`(cond
                 [(rhombus-expression (group pred ...)) rhs]
                 ...
                 [else (cond-fallthrough 'form-id '#,(syntax-srcloc (respan stx)))]))
            (for/fold ([si (extract-static-infos (car rhs-es))]) ([rhs-e (in-list (cdr rhs-es))])
              (static-infos-or si (extract-static-infos rhs-e)))))
         #'())]
       [(form-id (_::block))
        (values
         (relocate+reraw
          (respan stx)
          #`(cond-fallthrough 'form-id '#,(syntax-srcloc (respan stx))))
         #'())]))))

(define (cond-fallthrough who loc)
  (raise-srcloc-error who "no successful case" loc))

(define-syntax rhombus-when
  (expression-transformer
   (lambda (stx)
     (parse-when stx #'when))))

(define-syntax rhombus-unless
  (expression-transformer
   (lambda (stx)
     (parse-when stx #'unless))))

(define-for-syntax (parse-when stx racket-form-id)
  (syntax-parse stx
    [(form-id test ... (_::alts alt ...))
     (syntax-parse #'(alt ...)
       [((tag-thn::block thn ...))
        (values
         (relocate+reraw
          (respan stx)
          #`(#,racket-form-id (rhombus-expression (group test ...))
             (rhombus-body-at tag-thn thn ...)))
         #'())]
       [_
        (raise-syntax-error #f
                            "expected a single alternative"
                            stx)])]))
