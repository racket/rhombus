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
       (- (syntax-e #'rep.bind-depth)
          (syntax-e #'rep.use-depth))])))

(define-for-syntax (build-compound-repetition at-stx forms build-one
                                              #:is-sequence? [is-sequence? (lambda (form) #f)]
                                              #:maybe-immediate? [maybe-immediate? #f]
                                              #:extract [extract (lambda (form) form)])
  (define depths
    (for/list ([form (in-list forms)]
               [i (in-naturals)])
      (define depth (repetition-depth (extract form)))
      (if (is-sequence? form)
          (max 0 (sub1 depth))
          depth)))
  (define depth (apply max depths))
  (define-values (names lists use-depths list-depths immed?s)
    (for/lists (names lists use-depths list-depths immed?s)
               ([form (in-list forms)]
                [form-depth (in-list depths)])
      (define seq? (is-sequence? form))
      (syntax-parse (extract form)
        [rep::repetition-info
         (define list-depth (+ (min depth form-depth)
                               (if seq? 1 0)))
         (define e (repetition-as-list/non-immediate (extract form) list-depth))
         (values #'rep.name
                 e
                 form-depth
                 list-depth
                 (syntax-e #'rep.immediate?))])))
  (define immed? (and maybe-immediate?
                      (for/and ([immed? (in-list immed?s)]) immed?)))
  (define-values (list-e element-static-infos)
    (build-repetition-map depth
                          names lists use-depths list-depths immed?s
                          (if immed?
                              build-one
                              (lambda args
                                (define-values (body static-infos)
                                  (apply build-one args))
                                (values #`(lambda () #,body)
                                        static-infos)))))
  (make-repetition-info at-stx
                        (syntax/loc (syntax-parse at-stx
                                      [(x . _) #'x]
                                      [_ at-stx])
                          value)
                        list-e
                        depth
                        0
                        element-static-infos
                        immed?))

(define-for-syntax (build-repetition-map depth names lists depths list-depths immed?s build)
  (define top-depth depth)
  (define-values (body static-infos)
    (let loop ([depth depth]
               [depths depths]
               [named?s (for/list ([n (in-list names)])
                          #f)])
      (cond
        [(= 0 depth)
         ;; returns expression + static infos
         (apply build (for/list ([l-depth (in-list list-depths)]
                                 [name (in-list names)]
                                 [lst (in-list lists)]
                                 [named? (in-list named?s)]
                                 [immed? (in-list immed?s)])
                        (define e (if named? name lst))
                        (cond
                          [immed? e]
                          [else
                           (let loop ([e e] [depth (max 0 (- l-depth top-depth))])
                             (cond
                               [(= depth 0) #`(#,e)]
                               [else #`(for/list ([e (in-list #,e)])
                                         #,(loop #'e (sub1 depth)))]))])))]
        [else
         (define (wrap-body body)
           #`(for/list #,(for/list ([a-depth (in-list depths)]
                                    [name (in-list names)]
                                    [lst (in-list lists)]
                                    [named? (in-list named?s)]
                                    #:when (= a-depth depth))
                           #`[#,name (in-list #,(if named? name lst))])
               #,body))
         (define-values (body static-infos)
           (loop (sub1 depth)
                 (for/list ([a-depth (in-list depths)])
                   (min a-depth (sub1 depth)))
                 (for/list ([named? (in-list named?s)]
                            [a-depth (in-list depths)])
                   (if (= a-depth depth)
                       #t
                       named?))))
         (values (wrap-body body)
                 static-infos)])))
  (values body
          static-infos))
