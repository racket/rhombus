#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "with-syntax.rkt")
         "expression.rkt"
         "repetition.rkt")

(provide (for-syntax make-expression&repetition-prefix-operator
                     make-expression&repetition-infix-operator))

(begin-for-syntax
  (define (make-expression&repetition-prefix-operator name prec protocol exp)
    (define rep
      (lambda (form stx)
        (build-compound-repetition (list form) (lambda (form) (exp form stx)))))
    (make-expression+repetition-prefix-operator name prec protocol exp rep))

  (define (make-expression&repetition-infix-operator name prec protocol exp assc)
    (define rep
      (lambda (form1 form2 stx)
        (build-compound-repetition (list form1 form2) (lambda (form1 form2) (exp form1 form2 stx)))))
    (make-expression+repetition-infix-operator name prec protocol exp rep assc)))

(define-for-syntax (build-compound-repetition forms build-one)
  (define depths
    (for/list ([form (in-list forms)])
      (with-syntax-parse ([rep::repetition-info form])
        (- (syntax-e #'rep.bind-depth)
           (syntax-e #'rep.use-depth)))))
  (define depth (apply max depths))
  (make-repetition-info #'value
                        (map-repetition build-one
                                        depth
                                        (for/list ([form (in-list forms)]
                                                   [form-depth (in-list depths)])
                                          (with-syntax-parse ([rep::repetition-info form])
                                            (list #'rep.name
                                                  (repetition-as-list form
                                                                      (min depth form-depth))
                                                  form-depth
                                                  (syntax-e #'rep.immediate?)))))
                        depth
                        0
                        #'()
                        #f))

(define-for-syntax (map-repetition build depth name+list+depth+immed?s)
  (let ([names (map car name+list+depth+immed?s)]
        [lists (map cadr name+list+depth+immed?s)]
        [depths (map caddr name+list+depth+immed?s)]
        [immed?s (map cadddr name+list+depth+immed?s)])
    #`(let #,(for/list ([name (in-list names)]
                        [lst (in-list lists)]
                        [immed? (in-list immed?s)]
                        #:unless immed?)
               #`[#,name #,lst])
          #,(let loop ([depth depth]
                       [depths depths])
              (cond
                [(= 0 depth) (apply build (for/list ([name (in-list names)]
                                                     [lst (in-list lists)]
                                                     [immed? (in-list immed?s)])
                                            (if immed?
                                                lst
                                                name)))]
                [else
                 #`(for/list #,(for/list ([a-depth (in-list depths)]
                                          [name (in-list names)]
                                          #:when (= a-depth depth))
                                 #`[#,name (in-list #,name)])
                     #,(loop (sub1 depth)
                             (for/list ([a-depth (in-list depths)])
                               (min a-depth (sub1 depth)))))])))))
