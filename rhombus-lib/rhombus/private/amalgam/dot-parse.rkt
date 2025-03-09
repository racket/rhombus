#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "parens.rkt"
         (submod "assign.rkt" for-assign)
         (only-in "repetition.rkt"
                  identifier-repetition-use)
         "op-literal.rkt"
         "function-arity.rkt"
         "static-info.rkt")

(provide (for-syntax dot-parse-dispatch
                     set-parse-function-call!))

(define-for-syntax (dot-parse-dispatch k)
  (lambda (lhs dot field-stx tail more-static? repetition? success-k fail-k)
    (define (ary mask n-k no-k)
      (define (bad msg)
        (raise-syntax-error #f msg field-stx))
      (syntax-parse tail
        [((~and args (p-tag::parens g ...)) . new-tail)
         (define (success-call/static)
           (success-k (n-k #'(p-tag g ...)
                           (lambda (e)
                             (relocate+reraw
                              (respan (datum->syntax #f (list lhs dot field-stx #'args)))
                              e)))
                      #'new-tail))
         (define (success-call/dynamic)
           (success-k (no-k (lambda (e)
                              (relocate+reraw
                               (respan (datum->syntax #f (list lhs dot field-stx)))
                               e)))
                      tail))
         (define-values (n kws rsts? kwrsts?)
           (for/fold ([n 0] [kws null] [rsts? #f] [kwrsts? #f])
                     ([g (in-list (syntax->list #'(g ...)))])
             (syntax-parse g
               #:datum-literals (group op)
               [(group kw:keyword . _)
                (values n (cons #'kw kws) rsts? kwrsts?)]
               [(group _::&-expr . _)
                (values n kws #t kwrsts?)]
               [(group _::~&-expr . _)
                (values n kws rsts? #t)]
               [_
                (values (add1 n) kws rsts? kwrsts?)])))
         (cond
           [more-static?
            (if (check-arity field-stx #f mask n kws rsts? kwrsts? 'method #:always? #t)
                (success-call/static)
                (success-call/dynamic))]
           ;; dynamic
           [(check-arity #f #f mask n kws rsts? kwrsts? #f #:always? #t)
            (success-call/static)]
           [else
            (success-call/dynamic)])]
        [_
         (if more-static?
             (bad "expected parentheses afterward")
             (success-k (no-k (lambda (e)
                                (relocate+reraw
                                 (respan (datum->syntax #f (list lhs dot field-stx)))
                                 e)))
                        tail))]))

    (define (nary mask direct-id id)
      (define rator
        (cond
          [repetition? (identifier-repetition-use direct-id)]
          [else direct-id]))
      (ary mask
           (lambda (args reloc)
             (define-values (proc tail to-anon-function?)
               (parse-function-call rator (list lhs) #`(#,dot #,args)
                                    #:srcloc (reloc #'#f)
                                    #:static? more-static?
                                    #:can-anon-function? #t
                                    #:repetition? repetition?))
             proc)
           ;; return partially applied method
           (lambda (reloc)
             (reloc #`(#,id #,(discard-static-infos lhs))))))

    (define (just-access mk)
      (success-k (mk (discard-static-infos lhs)
                     (extract-static-infos lhs)
                     (lambda (e)
                       (relocate+reraw
                        (respan (datum->syntax #f (list lhs dot field-stx)))
                        e)))
                 tail))

    (define field
      (case-lambda
        [(mk)
         (just-access mk)]
        [(mk mk-set)
         (syntax-parse tail
           [assign::assign-op-seq
            (define-values (assign-expr tail)
              (build-assign
               (attribute assign.op)
               #'assign.op-name
               #'assign.name
               #`(lambda ()
                   #,(mk #'lhs
                         (extract-static-infos lhs)
                         (lambda (e)
                           (relocate+reraw
                            (respan (datum->syntax #f (list lhs dot field-stx)))
                            e))))
               #`(lambda (v)
                   #,(mk-set #'lhs
                             #'v
                             (lambda (e)
                               (relocate+reraw
                                (respan (datum->syntax #f (list lhs dot field-stx)))
                                e))))
               #'value
               #'assign.tail))
            (success-k #`(let ([lhs #,(discard-static-infos lhs)])
                           #,assign-expr)
                       tail)]
           [_
            (just-access mk)])]))

    (k (syntax-e field-stx) field ary nary repetition? fail-k)))

(define-for-syntax parse-function-call #f)
(define-for-syntax (set-parse-function-call! proc)
  (set! parse-function-call proc))
