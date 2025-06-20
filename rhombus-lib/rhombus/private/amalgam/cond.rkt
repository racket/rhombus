#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "expression.rkt"
         "parse.rkt"
         "else-clause.rkt"
         "static-info.rkt"
         "parens.rkt"
         "realm.rkt"
         "srcloc-error.rkt")

(provide (rename-out [rhombus-if if]
                     [rhombus-cond cond]
                     [rhombus-when when]
                     [rhombus-unless unless]))

(define-syntax rhombus-if
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id test ... (_::alts alt ...))
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
        (define els-e (syntax-parse #'e.parsed
                        #:literals (rhombus-body-at)
                        [(rhombus-body-at tag else-rhs ...)
                         (enforest-expression-block #'(tag else-rhs ...))]
                        [(rhombus-expression g)
                         (enforest-expression-block #'g)]
                        [_ #'e.parsed]))
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
