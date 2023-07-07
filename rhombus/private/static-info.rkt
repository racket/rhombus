#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     enforest/syntax-local
                     "introducer.rkt"
                     "srcloc.rkt")
         "expression.rkt"
         "name-root-ref.rkt"
         "indirect-static-info-key.rkt")

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
           static-info-get-stxs
           in-static-info-space
           wrap-static-info
           wrap-static-info*
           :static-info
           syntax-local-static-info
           syntax-local-static-info/indirect
           static-info/indirect
           extract-static-infos
           unwrap-static-infos
           relocate-wrapped
           static-info-lookup
           static-infos-intersect
           static-infos-union
           static-infos-remove
           make-static-infos
           install-static-infos!
           (rename-out [string-static-infos indirect-string-static-infos]
                       [bytes-static-infos indirect-bytes-static-infos])))

(provide define-static-info-syntax
         define-static-info-syntaxes
         define-static-info-syntax/maybe)

(module+ for-literal
  (provide (for-syntax quoted-static-infos)))

(begin-for-syntax
  (property static-info (get-stxs))

  (define in-static-info-space (make-interned-syntax-introducer/add 'rhombus/statinfo))

  (define (wrap-static-info expr key-id val-stx)
    (relocate+reraw
     expr
     #`(begin (quote-syntax (#,key-id #,val-stx))
              #,expr)))

  (define (wrap-static-info* expr stxes)
    (for/fold ([expr expr]) ([stx (in-list (if (syntax? stxes)
                                               (reverse (syntax->list stxes))
                                               stxes))])
      (syntax-parse stx
        [(key:identifier val) (wrap-static-info expr #'key #'val)])))

  (define-syntax-class (:static-info key-id)
    #:literals (begin quote-syntax quote)
    (pattern id:identifier
             #:do [(define v (syntax-local-value* (in-static-info-space
                                                   (out-of-expression-space #'id))
                                                  static-info-ref))
                   (define val (and v
                                    (static-info-lookup ((static-info-get-stxs v)) key-id)))]
             #:when val
             #:attr val val)
    (pattern (begin (quote-syntax (~and form (key:identifier val))) _)
             #:when (free-identifier=? #'key key-id))
    (pattern (begin (quote-syntax (~and form (key:identifier indirect-id))) _)
             #:when (free-identifier=? #'key #'#%indirect-static-info)
             #:do [(define v (indirect-static-info-ref #'indirect-id key-id))]
             #:when v
             #:attr val v)
    (pattern (begin (quote-syntax _) (~var e (:static-info key-id)))
             #:attr val #'e.val)
    (pattern (quote d)
             #:do [(define si (quoted-static-infos #'d))
                   (define v (static-info-lookup si key-id))]
             #:when v
             #:attr val v))

  (define (syntax-local-static-info expr key-id)
    (syntax-parse expr
      [(~var dp (:static-info key-id))
       #'dp.val]
      [_ #f]))

  (define (indirect-static-info-ref id key-id)
    (syntax-local-static-info id key-id))

  (define string-static-infos #f)
  (define bytes-static-infos #f)
  (define (install-static-infos! kind static-infos)
    (case kind
      [(string) (set! string-static-infos static-infos)]
      [(bytes) (set! bytes-static-infos static-infos)]
      [else (void)]))

  (define (extract-static-infos e)
    (syntax-parse e
      #:literals (begin quote-syntax quote)
      [id:identifier
       (define v (syntax-local-value* (in-static-info-space #'id)
                                      static-info-ref))
       (if v
           ((static-info-get-stxs v))
           null)]
      [(begin (quote-syntax (~and form (key:identifier val))) e)
       (cons #'form (let ([si (extract-static-infos #'e)])
                      (if (syntax? si)
                          (syntax->list si)
                          si)))]
      [(quote d) (quoted-static-infos #'d)]
      [_ null]))
  
  (define (quoted-static-infos d)
    (cond
      [(string? (syntax-e d)) string-static-infos]
      [(bytes? (syntax-e d)) bytes-static-infos]
      [else null]))

  (define (unwrap-static-infos e)
    (syntax-parse e
      #:literals (begin quote-syntax)
      [(begin (quote-syntax (~and form (key:identifier val))) e)
       (unwrap-static-infos #'e)]
      [_ e]))

  (define (static-info-lookup static-infos find-key)
    (for/or ([static-info (in-list (if (syntax? static-infos)
                                       (syntax->list static-infos)
                                       static-infos))])
      (syntax-parse static-info
        [(key val) (or (and (free-identifier=? #'key find-key)
                            #'val)
                       (and (free-identifier=? #'key #'#%indirect-static-info)
                            (indirect-static-info-ref #'val find-key)))]
        [_ #f])))

  (define (syntax-local-static-info/indirect e key indirect-key)
    (or (syntax-local-static-info e key)
        (let ([id (syntax-local-static-info e indirect-key)])
          (and id
               (syntax-local-static-info/indirect id key indirect-key)))))

  (define (static-info/indirect indexable-static-info key indirect-key)
    (or (indexable-static-info key)
        (let ([id (indexable-static-info indirect-key)])
          (and id
               (syntax-local-static-info/indirect id key indirect-key)))))

  ;; it's better to relocate and then wrap, since wrapping propagates
  ;; the location, but sometimes it's so much easier to relocate
  ;; afterward that it's worth the extra cost
  (define (relocate-wrapped srcloc e)
    (syntax-parse e
      #:literals (begin quote-syntax)
      [((~and tag begin) (~and qs (quote-syntax (key:identifier val))) e)
       (define e2 (relocate-wrapped srcloc #'e))
       (relocate+reraw e2 #`(tag qs #,e2))]
      [_ (relocate+reraw srcloc e)])))

(define-syntax (define-static-info-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs ...)
     #`(define-syntax #,(in-static-info-space #'id)
         (static-info (lambda () (list (quasisyntax rhs) ...))))]))

(define-syntax (define-static-info-syntaxes stx)
  (syntax-parse stx
    [(_ (id:identifier ...) rhs ...)
     #'(begin
         (define-static-info-syntax id rhs ...)
         ...)]))

(define-syntax (define-static-info-syntax/maybe stx)
  (syntax-parse stx
    [(_ id) #'(begin)]
    [(_ id rhs ...) #'(define-static-info-syntax id rhs ...)]))

(define-for-syntax (make-static-infos static-infos)
  (define infos (syntax->list static-infos))
  (static-info (lambda () infos)))

(define-for-syntax (static-infos-intersect as bs)
  (let ([bs (syntax->list bs)])
    (for/list ([a (in-list (syntax->list as))]
               #:when (syntax-parse a
                        [(a-key a-val)
                         (for/or ([b (in-list bs)])
                           (syntax-parse b
                             [(b-key b-val)
                              (and (free-identifier=? #'a-key #'b-key)
                                   (equal-static-info-value? #'a-val #'b-val))]
                             [_ #f]))]
                        [_ #f]))
      a)))

(define-for-syntax (static-infos-union as bs)
  (append (syntax->list as) bs))

(define-for-syntax (static-infos-remove as key)
  (for/list ([a (in-list (if (syntax? as) (syntax->list as) as))]
             #:when (syntax-parse a
                      [(a-key a-val) (not (free-identifier=? #'a-key key))]))
    a))

(define-for-syntax (equal-static-info-value? a b)
  (cond
    [(identifier? a)
     (and (identifier? b)
          (free-identifier=? a b))]
    [(identifier? b) #f]
    [(syntax? a)
     (equal-static-info-value? (syntax-e a) b)]
    [(syntax? b)
     (equal-static-info-value? a (syntax-e b))]
    [(null? a) (null? b)]
    [(pair? a)
     (and (pair? b)
          (and (equal-static-info-value? (car a) (car b))
               (equal-static-info-value? (cdr a) (cdr b))))]
    [(vector? a)
     (and (vector? b)
          (= (vector-length a) (vector-length b))
          (for/and ([ae (in-vector a)]
                    [be (in-vector b)])
            (equal-static-info-value? (car ae) (car be))))]
    [(box? a)
     (and (box? b)
          (equal-static-info-value? (unbox a) (unbox b)))]
    [else (equal? a b)]))
