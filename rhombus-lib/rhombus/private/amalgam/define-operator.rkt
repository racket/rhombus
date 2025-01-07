#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "repetition.rkt"
         "compound-repetition.rkt"
         "static-info.rkt"
         "rhombus-primitive.rkt"
         "flonum-key.rkt"
         "fixnum-key.rkt"
         "parse.rkt"
         "order.rkt")

(provide define-prefix
         define-infix

         (for-syntax prefix
                     infix))

(begin-for-syntax
  (require (for-syntax racket/base
                       syntax/parse/pre))

  (define-syntax (prefix stx)
    (syntax-parse stx
      [(_ prim:identifier
          (~optional (~seq #:order order-id:identifier))
          (~optional (~seq #:precedences precedences-expr))
          (~optional (~seq #:weaker-than (weaker-op ...))
                     #:defaults ([(weaker-op 1) '()]))
          (~optional (~seq #:same-as (same-op ...))
                     #:defaults ([(same-op 1) '()]))
          (~optional (~seq #:same-on-right-as (same-on-right-op ...))
                     #:defaults ([(same-on-right-op 1) '()]))
          (~optional (~seq #:same-on-left-as (same-on-left-op ...))
                     #:defaults ([(same-on-left-op 1) '()]))
          (~optional (~seq #:stronger-than (stronger-op ...))
                     #:defaults ([(stronger-op 1) '()]))
          (~optional (~seq #:static-infos statinfos)
                     #:defaults ([statinfos #'()]))
          (~optional (~seq #:flonum flprim:identifier flonum-statinfos)
                     #:defaults ([flprim #'#f]
                                 [flonum-statinfos #'()]))
          (~optional (~seq #:fixnum fxprim:identifier fixnum-statinfos)
                     #:defaults ([fxprim #'#f]
                                 [fixnum-statinfos #'()])))
       #`(make-expression&repetition-prefix-operator
          (~? (lambda () (order-quote order-id))
              #f)
          (~? precedences-expr
              (lambda ()
                (list (cons (quote-syntax weaker-op)
                            'weaker)
                      ...
                      (cons (quote-syntax same-op)
                            'same)
                      ...
                      (cons (quote-syntax same-on-right-op)
                            'same-on-right)
                      ...
                      (cons (quote-syntax same-on-left-op)
                            'same-on-left)
                      ...
                      (cons (quote-syntax stronger-op)
                            'stronger)
                      ...)))
          'prefix
          #:element-statinfo? #t
          (lambda (form stx)
            #,@(if (syntax-e #'flprim)
                   #`((define flonum? (flonum-statinfo? form)))
                   #`())
            #,@(if (syntax-e #'fxprim)
                   #`((define fixnum? (fixnum-statinfo? form)))
                   #`())
            (wrap-static-info*
             (relocate+reraw (respan (datum->syntax #f (list stx form)))
                             (datum->syntax (quote-syntax here)
                                            (list (relocate-id stx
                                                               #,(if (syntax-e #'flprim)
                                                                     #`(if flonum?
                                                                           (quote-syntax flprim)
                                                                           #,(if (syntax-e #'fxprim)
                                                                                 #`(if fixnum?
                                                                                       (quote-syntax fxprim)
                                                                                       (quote-syntax prim))
                                                                                 #`(quote-syntax prim)))
                                                                     #`(quote-syntax prim)))
                                                  (discard-static-infos form))
                                            #f
                                            stx))
             #,(if (syntax-e #'flprim)
                   #`(if flonum?
                         #`flonum-statinfos
                         #`statinfos)
                   #`#`statinfos))))]))

  (define-syntax (infix stx)
    (syntax-parse stx
      [(_ prim:identifier
          (~optional (~seq #:order order-id:identifier))
          (~optional (~seq #:precedences precedences-expr))
          (~optional (~seq #:weaker-than (weaker-op ...))
                     #:defaults ([(weaker-op 1) '()]))
          (~optional (~seq #:same-as (same-op ...))
                     #:defaults ([(same-op 1) '()]))
          (~optional (~seq #:same-on-left-as (same-on-left-op ...))
                     #:defaults ([(same-on-left-op 1) '()]))
          (~optional (~seq #:stronger-than (stronger-op ...))
                     #:defaults ([(stronger-op 1) '()]))
          (~optional (~seq #:associate assoc)
                     #:defaults ([assoc #''left]))
          (~optional (~seq #:static-infos statinfos)
                     #:defaults ([statinfos #'()]))
          (~optional (~seq #:flonum flprim:identifier flonum-statinfos)
                     #:defaults ([flprim #'#f]
                                 [flonum-statinfos #'()]))
          (~optional (~seq #:fixnum fxprim:identifier fixnum-statinfos)
                     #:defaults ([fxprim #'#f]
                                 [fixnum-statinfos #'()])))
       #`(make-expression&repetition-infix-operator
          (~? (lambda () (order-quote order-id))
              #f)
          (~? precedences-expr
              (lambda ()
                (list (cons (quote-syntax weaker-op)
                            'weaker)
                      ...
                      (cons (quote-syntax same-op)
                            'same)
                      ...
                      (cons (quote-syntax same-on-left-op)
                            'same-on-left)
                      ...
                      (cons (quote-syntax stronger-op)
                            'stronger)
                      ...)))
          'infix
          #:element-statinfo? #t
          (lambda (form1 form2 stx)
            #,@(if (syntax-e #'flprim)
                   #`((define flonum? (and (flonum-statinfo? form1)
                                           (flonum-statinfo? form2))))
                   #`())
            #,@(if (syntax-e #'fxprim)
                   #`((define fixnum? (and (fixnum-statinfo? form1)
                                           (fixnum-statinfo? form2))))
                   #`())
            (wrap-static-info*
             (relocate+reraw (respan (datum->syntax #f (list form1 stx form2)))
                             (datum->syntax (quote-syntax here)
                                            (list (relocate-id stx
                                                               #,(if (syntax-e #'flprim)
                                                                     #`(if flonum?
                                                                           (quote-syntax flprim)
                                                                           #,(if (syntax-e #'fxprim)
                                                                                 #`(if fixnum?
                                                                                       (quote-syntax fxprim)
                                                                                       (quote-syntax prim))
                                                                                 #`(quote-syntax prim)))
                                                                     #`(quote-syntax prim)))
                                                  (discard-static-infos form1)
                                                  (discard-static-infos form2))
                                            #f
                                            stx))

             #,(if (syntax-e #'flprim)
                   #`(if flonum?
                         #`flonum-statinfos
                         #`statinfos)
                   #`#`statinfos)))
          assoc)])))

(define-syntax (define-prefix stx)
  (syntax-parse stx
    [(_ (~optional (~seq (~and #:who
                               (~bind [who? #t]))
                         (~optional ext-name:identifier)))
        name:identifier prim:identifier
        spec ...)
     #`(begin
         #,@(if (attribute who?)
                (list #`(void (set-primitive-who! 'prim '(~? ext-name name))))
                '())
         (define-syntaxes (name #,(in-repetition-space #'name))
           (prefix prim spec ...)))]))

(define-syntax (define-infix stx)
  (syntax-parse stx
    [(_ (~optional (~seq (~and #:who
                               (~bind [who? #t]))
                         (~optional ext-name:identifier)))
        name:identifier prim:identifier
        spec ...)
     #`(begin
         #,@(if (attribute who?)
                (list #`(void (set-primitive-who! 'prim '(~? ext-name name))))
                '())
         (define-syntaxes (name #,(in-repetition-space #'name))
           (infix prim spec ...)))]))
