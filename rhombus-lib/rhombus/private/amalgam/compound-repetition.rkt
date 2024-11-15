#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "repetition.rkt")

(provide (for-syntax make-expression&repetition-prefix-operator
                     make-expression&repetition-infix-operator

                     repetition-depth

                     build-compound-repetition))

(begin-for-syntax
  (define (make-expression&repetition-prefix-operator prec protocol exp)
    (when (eq? protocol 'macro) (error "macro protocol not currently supported for prefix repetition"))
    (define rep
      (lambda (form self-stx)
        (build-compound-repetition self-stx
                                   (list form)
                                   (lambda (form) (values (exp form self-stx)
                                                          #'())))))
    (values
     (expression-prefix-operator prec protocol exp)
     (repetition-prefix-operator (add-repet-space prec) protocol rep)))

  (define (make-expression&repetition-infix-operator prec protocol exp assc)
    (define rep
      (if (eq? protocol 'macro)
          ;; used for postfix
          (lambda (form stx)
            (define tail #f)
            (define e (build-compound-repetition stx
                                                 (list form)
                                                 (lambda (form)
                                                   (define-values (e e-tail) (exp form stx))
                                                   (set! tail e-tail)
                                                   (values e #'()))))
            (values e tail))
          (lambda (form1 form2 self-stx)
            (build-compound-repetition self-stx
                                       (list form1 form2)
                                       (lambda (form1 form2) (values (exp form1 form2 self-stx)
                                                                     #'()))))))
    (values
     (expression-infix-operator prec protocol exp assc)
     (repetition-infix-operator (add-repet-space prec) protocol rep assc)))

  (define (add-repet-space get-prec)
    (lambda ()
      (let ([prec (if (procedure? get-prec)
                      (get-prec)
                      get-prec)])
        (for/list ([p (in-list prec)])
          (if (identifier? (car p))
              (cons (in-repetition-space (car p)) (cdr p))
              p)))))

  (define (repetition-depth form)
    (syntax-parse form
      [rep::repetition-info
       (length (syntax->list #'rep.for-clausess))])))

(define-for-syntax (build-compound-repetition at-stx forms build-one
                                              #:sequence-for-form [sequence-for-form #'for/list]
                                              #:is-sequence? [is-sequence? (lambda (form) #f)]
                                              #:extract [extract (lambda (form) form)])
  (define depth
    (for/fold ([depth 0]) ([form (in-list forms)])
      (max depth (- (repetition-depth (extract form))
                    (if (is-sequence? form) 1 0)))))
  (define-values (for-clausesss bodys)
    (for/lists (bodys for-clausesss)
               ([form (in-list forms)])
      (syntax-parse (extract form)
        [rep::repetition-info
         (define for-clausess (syntax->list #'rep.for-clausess))
         (define (add-disappeared stx)
           (add-repetition-disappeared stx #'rep.rep-expr))
         (cond
           [(is-sequence? form)
            (define rev-for-clausess (reverse for-clausess))
            (values (reverse (cdr rev-for-clausess))
                    (add-disappeared
                     #`(#,sequence-for-form #,(car rev-for-clausess)
                        rep.body)))]
           [else
            (values for-clausess
                    (add-disappeared #'rep.body))])])))
  (define-values (body static-infos)
    (apply build-one bodys))
  (make-repetition-info at-stx
                        (let loop ([depth depth]
                                   [for-clausesss for-clausesss])
                          (cond
                            [(zero? depth)
                             null]
                            [else
                             (cons
                              (apply
                               append
                               (for/list ([for-clausess (in-list for-clausesss)]
                                          #:when (= (length for-clausess) depth))
                                 (syntax->list (car for-clausess))))
                              (loop (sub1 depth)
                                    (for/list ([for-clausess (in-list for-clausesss)])
                                      (if (= (length for-clausess) depth)
                                          (cdr for-clausess)
                                          for-clausess))))]))
                        body
                        static-infos
                        0))
