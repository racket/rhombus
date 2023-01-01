#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "with-syntax.rkt")
         "expression.rkt"
         "repetition.rkt"
         "static-info.rkt")

(provide (for-syntax make-expression&repetition-prefix-operator
                     make-expression&repetition-infix-operator

                     repetition-depth

                     build-compound-repetition))

(begin-for-syntax
  (define (make-expression&repetition-prefix-operator name prec protocol exp)
    (define rep
      (lambda (form stx)
        (build-compound-repetition stx
                                   (list form)
                                   (lambda (form) (values (exp form stx)
                                                          #'())))))
    (make-expression+repetition-prefix-operator name prec protocol exp rep))

  (define (make-expression&repetition-infix-operator name prec protocol exp assc)
    (define rep
      (lambda (form1 form2 stx)
        (build-compound-repetition stx
                                   (list form1 form2)
                                   (lambda (form1 form2) (values (exp form1 form2 stx)
                                                                 #'())))))
    (make-expression+repetition-infix-operator name prec protocol exp rep assc))

  (define (repetition-depth form)
    (with-syntax-parse ([rep::repetition-info form])
      (- (syntax-e #'rep.bind-depth)
         (syntax-e #'rep.use-depth)))))

(define-for-syntax (build-compound-repetition at-stx forms build-one
                                              #:is-sequence? [is-sequence? (lambda (form) #f)]
                                              #:extract [extract (lambda (form) form)])
  (define depths
    (for/list ([form (in-list forms)]
               [i (in-naturals)])
      (define depth (repetition-depth (extract form)))
      (if (is-sequence? form)
          (max 0 (sub1 depth))
          depth)))
  (define depth (apply max depths))
  (define-values (names lists use-depths immed?s)
    (for/lists (names lists depths immed?s) ([form (in-list forms)]
                                             [form-depth (in-list depths)])
      (define seq? (is-sequence? form))
      (with-syntax-parse ([rep::repetition-info (extract form)])
        (values #'rep.name
                (repetition-as-list (extract form)
                                    (+ (min depth form-depth)
                                       (if seq? 1 0)))
                form-depth
                (if seq?
                    #f
                    (syntax-e #'rep.immediate?))))))
  (define-values (list-e element-static-infos)
    (build-repetition-map depth
                          names lists use-depths immed?s
                          build-one))
  (make-repetition-info at-stx
                        (syntax/loc (syntax-parse at-stx
                                      [(x . _) #'x]
                                      [_ at-stx])
                          value)
                        list-e
                        depth
                        0
                        element-static-infos
                        #f))

(define-for-syntax (build-repetition-map depth names lists depths immed?s build)
  (define (wrap-body body)
    #`(let #,(for/list ([name (in-list names)]
                        [lst (in-list lists)]
                        [immed? (in-list immed?s)]
                        #:unless immed?)
               #`[#,name #,lst])
          #,body))
  (define-values (body static-infos)
    (let loop ([depth depth]
               [depths depths])
      (cond
        [(= 0 depth)
         ;; returns expression + static infos
         (apply build (for/list ([name (in-list names)]
                                 [lst (in-list lists)]
                                 [immed? (in-list immed?s)])
                        (if immed?
                            lst
                            name)))]
        [else
         (define (wrap-body body)
           #`(for/list #,(for/list ([a-depth (in-list depths)]
                                    [name (in-list names)]
                                    #:when (= a-depth depth))
                           #`[#,name (in-list #,name)])
               #,body))
         (define-values (body static-infos)
           (loop (sub1 depth)
                 (for/list ([a-depth (in-list depths)])
                   (min a-depth (sub1 depth)))))
         (values (wrap-body body)
                 static-infos)])))
  (values (wrap-body body)
          static-infos))
