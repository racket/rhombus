#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         "expression.rkt"
         "parse.rkt"
         "parens.rkt"
         "annotation-failure.rkt"
         "error-adjust.rkt")

(provide Port.Input.using
         Port.Output.using)

(define-for-syntax (build-using who what
                                port? close param
                                pres make-e blk)
  (syntax-parse blk
    [(tag::block body ...+)
     #:do [(define pre-ids (generate-temporaries pres))]
     #:with (pre ...) pres
     #:with (pre-id ...) pre-ids
     (values
      #`(call-with-continuation-barrier
         (lambda ()
           (let ([p #f]
                 [pre-id pre]
                 ...)
             (#||# dynamic-wind
              (lambda ()
                (set! p #,(make-e pre-ids))
                #,@(if port?
                       (list #`(unless (#,port? p)
                                 (raise-annotation-failure '#,who '#,what p)))
                       '()))
              (lambda ()
                (parameterize ([#,param p])
                  (rhombus-body-at tag body ...)))
              (lambda ()
                (#,close p))))))
      #'())]
    [(tag::block)
     (raise-syntax-error who "empty body" blk)]))

(define-syntax Port.Input.using
  (expression-transformer
   (lambda (stx)
     (define (build pre make-e blk checked?)
       (build-using 'Port.Input.using "Port.Input"
                    (and checked? #'input-port?) #'close-input-port #'current-input-port
                    pre make-e blk))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ #:file t ... (~and blk (_::block . _)))
        (build (list #`(rhombus-expression (#,group-tag t ...)))
               (lambda (pre-ids)
                 #`(open-input-file* #,(car pre-ids)))
               #'blk
               #f)]
       [(_ t ... (~and blk (_::block body . _)))
        (build '()
               (lambda (pre-ids)
                 #`(rhombus-expression (#,group-tag t ...)))
               #'blk
               #t)]))))

(define-syntax Port.Output.using
  (expression-transformer
   (lambda (stx)
     (define (build pre make-e blk checked?)
       (build-using 'Port.Output.using "Port.Output"
                    (and checked? #'output-port?) #'close-output-port #'current-output-port
                    pre make-e blk))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ #:file t ... (tag::block
                         (group #:exists (xtag::block xbody ...+))
                         body ...))
        (build (list #`(rhombus-expression (#,group-tag t ...))
                     #'(rhombus-body-at xtag xbody ...))
               (lambda (pre-ids)
                 #`(open-output-file* #,(car pre-ids) #,(cadr pre-ids)))
               (datum->syntax #f (syntax-e #'(tag body ...)))
               #f)]
       [(_ #:file t ... (tag::block
                         (group #:exists tx ...+)
                         body ...))
        (build (list #`(rhombus-expression (#,group-tag t ...))
                     #`(rhombus-expression (#,group-tag tx ...)))
               (lambda (pre-ids)
                 #`(open-output-file* #,(car pre-ids) #,(cadr pre-ids)))
               (datum->syntax #f (syntax-e #'(tag body ...)))
               #f)]
       [(_ #:file t ... (~and blk (_::block . _)))
        (build (list #`(rhombus-expression (#,group-tag t ...)))
               (lambda (pre-ids)
                 #`(open-output-file* #,(car pre-ids) 'error))
               #'blk
               #f)]
       [(_ t ... (~and blk (_::block . _)))
        (build '()
               (lambda (pre-ids)
                 #`(rhombus-expression (#,group-tag t ...)))
               #'blk
               #t)]))))

(define (open-input-file* p)
  (with-error-adjust-primitive ([open-input-file Port.Input.using])
    (open-input-file p)))

(define (open-output-file* p exists)
  (with-error-adjust-primitive ([open-input-file Port.Output.using])
    (open-output-file p #:exists exists)))
