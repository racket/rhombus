#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "parse.rkt"
         "parens.rkt"
         "annotation-failure.rkt"
         "error-adjust.rkt")

(provide Port.Input.using
         Port.Output.using)

(define-for-syntax (build-using who what port? close param pre e blk)
  (syntax-parse blk
    [(tag::block body ...+)
     (values
      #`(call-with-continuation-barrier
         (lambda ()
           (let ([p #f]
                 [pre #,pre])
             (#||# dynamic-wind
              (lambda ()
                (set! p #,e)
                (unless (#,port? p)
                  (raise-annotation-failure '#,who '#,what p)))
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
     (define (build pre e blk pred)
       (build-using 'Port.Input.using "Port.Input" pred #'close-input-port #'current-input-port pre e blk))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ #:file t ... (~and blk (_::block . _)))
        #:with e::expression #'(group t ...)
        (build #'e.parsed #'(open-input-file* pre) #'blk #'(lambda (x) #t))]
       [(_ t ... (~and blk (_::block body . _)))
        #:with e::expression #'(group t ...)
        (build #'#f #'e.parsed #'blk #'input-port?)]))))

(define-syntax Port.Output.using
  (expression-transformer
   (lambda (stx)
     (define (build pre e blk pred)
       (build-using 'Port.Output.using "Port.Output" pred #'close-output-port #'current-output-port pre e blk))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ #:file t ... (tag::block
                         (group #:exists (xtag::block xbody ...+))
                         body ...))
        #:with e::expression #'(group t ...)
        (build #'(rhombus-body-at xtag xbody ...)
               #'(open-output-file* e.parsed pre)
               (datum->syntax #f (syntax-e #'(tag body ...)))
               #'(lambda (x) #t))]
       [(_ #:file t ... (tag::block
                         (group #:exists tx ...+)
                         body ...))
        #:with e::expression #'(group t ...)
        #:with ex::expression #'(group tx ...)
        (build #'(cons e.parsed ex.parsed)
               #'(open-output-file* (car pre) (cdr pre))
               (datum->syntax #f (syntax-e #'(tag body ...)))
                #'(lambda (x) #t))]
       [(_ #:file t ... (~and blk (_::block . _)))
        #:with e::expression #'(group t ...)
        (build #'e.parsed #'(open-output-file* pre 'error) #'blk #'(lambda (x) #t))]
       [(_ t ... (~and blk (_::block . _)))
        #:with e::expression #'(group t ...)
        (build #'#f #'e.parsed #'blk #'output-port?)]))))

(define (open-input-file* p)
  (with-error-adjust-primitive ([open-input-file Port.Input.using])
    (open-input-file p)))

(define (open-output-file* p exists)
  (with-error-adjust-primitive ([open-input-file Port.Output.using])
    (open-output-file p #:exists exists)))
