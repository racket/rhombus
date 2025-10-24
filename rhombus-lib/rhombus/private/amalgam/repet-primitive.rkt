#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "repetition.rkt"
         "parse.rkt"
         "static-info.rkt"
         "sequence-constructor-key.rkt"
         "index-result-key.rkt"
         "annotation-failure.rkt"
         "parens.rkt"
         "number.rkt")

(provide (for-space rhombus/repet
                    index
                    each))

(module+ for-sequence-check
  (provide check-sequence-for-each))

(define-repetition-syntax index
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (~and args (_::parens r::repetition)) . tail)
        (values (make-index-repetition #'form-id
                                       #'args
                                       #'r.parsed
                                       1)
                #'tail)]
       [(form-id (~and args (_::parens r::repetition (group depth:integer))) . tail)
        #:when ((syntax-e #'depth) . > . 0)
        (values (make-index-repetition #'form-id
                                       #'args
                                       #'r.parsed
                                       (syntax-e #'depth))
                #'tail)]
       [(form-id (~and args (_::parens r::repetition g)) . tail)
        (raise-syntax-error #f
                            "expected a literal positive integer"
                            (respan (datum->syntax #F (list #'form-id #'args)))
                            #'g)]
       [(form-id args . tail)
        (raise-syntax-error #f
                            "expected a parenthesized repetition"
                            (respan (datum->syntax #f (list #'form-id #'args)))
                            #'args)]))))

(define-for-syntax (make-index-repetition form-id args rep-parsed depth)
  (syntax-parse rep-parsed
    [r::repetition-info
     (define for-clausess (syntax->list #'r.for-clausess))
     (unless (depth . <= . (length for-clausess))
       (raise-syntax-error #f
                           (format "expected a repetition of at least depth ~a" depth)
                           (respan (datum->syntax #f (list #'form-id #'args)))
                           #'r.rep-expr))
     (make-repetition-info (respan (datum->syntax #f (list form-id args)))
                           (reverse
                            (let loop ([rev-clausess (reverse for-clausess)]
                                       [depth depth])
                              (cond
                                [(= depth 1)
                                 (cons
                                  (add-indexing-to-clause #'pos (syntax->list (car rev-clausess)))
                                  (cdr rev-clausess))]
                                [else
                                 (cons (car rev-clausess)
                                       (loop (cdr rev-clausess) (sub1 depth)))])))
                           #'pos
                           (get-int-static-infos)
                           #'r.used-depth)]))

(define-for-syntax (add-indexing-to-clause pos-id clauses)
  ;; use `in-indexed` instead of adding a parallel `in-naturals` so that
  ;; rendering the repeition with `#:on-length-mismatch` will be ok;
  ;; in the common case, that will be optimized to a parallel `in-naturals`
  (define (index-clause clause)
    (syntax-parse clause
      [[(id) rhs] #`[(id #,pos-id) (in-indexed rhs)]]
      [_ #f]))
  (cond
    [(ormap index-clause clauses)
     ;; some clause can support an index, so add it to the first one available:
     (let loop ([clauses clauses])
       (cond
         [(index-clause (car clauses))
          => (lambda (new-clause)
               (cons new-clause (cdr clauses)))]
         [else (cons (car clauses) (loop (cdr clauses)))]))]
    [(null? clauses)
     (error "index: repetition has no clauses")]
    [else
     ;; unusual that there's no single-values clause, but we can handle
     ;; it by using the more expensive `in-parallel-values` form
     (cons (syntax-parse (car clauses)
             [[(id ...) rhs] #`[(id ... #,pos-id) (in-parallel-values #,(length (syntax->list #'(id ...)))
                                                                      rhs
                                                                      1
                                                                      (in-naturals 0))]])
           (cdr clauses))]))

(define-repetition-syntax each
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id . tail)
        #:with e::expression #'(group . tail)
        (define seq-ctr-id (syntax-local-static-info #'e.parsed #'#%sequence-constructor))
        (define e-plain (discard-static-infos #'e.parsed))
        (values (make-repetition-info (respan (datum->syntax #f (list #'form-id #'args)))
                                      #`(([(repet) #,(if seq-ctr-id
                                                         #`(#,seq-ctr-id #,e-plain)
                                                         #`(check-sequence-for-each 'form-id #,e-plain))]))
                                      #'repet
                                      (or (extract-index-uniform-result
                                           (syntax-local-static-info #'e.parsed #'#%index-result))
                                          #'())
                                      0)
                #'())]))))

(define (check-sequence-for-each who v)
  (if (and (sequence? v) (not (exact-integer? v)))
      v
      (raise-annotation-failure who v "Sequence")))
