#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/property
                     enforest/syntax-local)
         "expression.rkt"
         "name-root-ref.rkt")

;; Represent static information in either of two ways:
;;
;;   - bind an identifier (in some space) to a `static-info` record,
;;     which maps keys to values; the static info applies to a use of
;;     the identifier
;;
;;   - wrap a form with `(begin (quote-syntax (key val)) form)`, where
;;     the pattern can be nested in `form`, and the static info
;;     applies to the form

(begin-for-syntax
  (provide (property-out static-info)
           in-static-info-space
           wrap-static-info
           wrap-static-info*
           :static-info
           syntax-local-static-info
           extract-static-infos
           unwrap-static-infos
           static-info-lookup))

(provide define-static-info-syntax
         define-static-info-syntax/maybe)

(begin-for-syntax
  (property static-info (stxs))

  (define in-static-info-space (make-interned-syntax-introducer 'rhombus/static-info))
    
  (define (wrap-static-info expr key-id val-stx)
    (quasisyntax/loc expr
      (begin (quote-syntax (#,key-id #,val-stx))
             #,expr)))

  (define (wrap-static-info* expr stxes)
    (for/fold ([expr expr]) ([stx (in-list (syntax->list stxes))])
      (syntax-parse stx
        [(key:identifier val) (wrap-static-info expr #'key #'val)])))

  (define-syntax-class (:static-info key-id)
    #:literals (begin quote-syntax)
    (pattern id:identifier
             #:do [(define v (syntax-local-value* (in-static-info-space #'id)
                                                  (lambda (v)
                                                    (name-root-ref-root v static-info-ref))))
                   (define val (and v
                                    (for/or ([form (in-list (static-info-stxs v))])
                                      (syntax-parse form
                                        [(key:identifier val)
                                         #:when (free-identifier=? #'key key-id)
                                         #'val]
                                        [_ #f]))))]
             #:when val
             #:attr val val)
    (pattern (begin (quote-syntax (~and form (key:identifier val))) _)
             #:when (free-identifier=? #'key key-id))
    (pattern (begin (quote-syntax _) (~var e (:static-info key-id)))
             #:attr val #'e.val))

  (define (syntax-local-static-info expr key-id)
    (syntax-parse expr
      [(~var dp (:static-info key-id))
       #'dp.val]
      [_ #f]))

  (define (extract-static-infos e)
    (syntax-parse e
      #:literals (begin quote-syntax)
      [id:identifier
       (define v (syntax-local-value* (in-static-info-space #'id)
                                      (lambda (v)
                                        (name-root-ref-root v static-info-ref))))
       (if v
           (static-info-stxs v)
           null)]
      [(begin (quote-syntax (~and form (key:identifier val))) e)
       (cons #'form (extract-static-infos #'e))]
      [_ null]))

  (define (unwrap-static-infos e)
    (syntax-parse e
      #:literals (begin quote-syntax)
      [(begin (quote-syntax (~and form (key:identifier val))) e)
       (unwrap-static-infos #'e)]
      [_ e]))

  (define (static-info-lookup static-infos find-key)
    (for/or ([static-info (in-list (syntax->list static-infos))])
      (syntax-parse static-info
        [(key val) (and (free-identifier=? #'key find-key)
                        #'val)]
        [_ #f]))))

(define-syntax (define-static-info-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs ...)
     #`(define-syntax #,(in-static-info-space #'id)
         (static-info (list (quote-syntax rhs) ...)))]))

(define-syntax (define-static-info-syntax/maybe stx)
  (syntax-parse stx
    [(_ id) #'(begin)]
    [(_ id rhs ...) #'(define-static-info-syntax id rhs ...)]))
