#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt"
                     "srcloc.rkt"
                     "entry-point-adjustment.rkt"
                     "class-transformer-rhs.rkt"
                     "pack.rkt")
         "macro-macro.rkt"
         "pack.rkt"
         "parens.rkt")

(provide (for-syntax wrap-class-transformer))

(define-for-syntax (wrap-class-transformer name tail-name g make-prefix-operator what
                                           #:extra-args [extra-args null])
  (check-class-transformer tail-name g what)
  (define lam
    (syntax-parse g
      #:datum-literals (group named-macro)
      [(named-macro (form-id . _)
                    (q-tag::quotes ((~and g-tag group) id:identifier . pat))
                    blk)
       #:with empty-parens (datum->syntax #'id '(parens) #'id #'id)
       (define stx
         (respan #`(form-id
                    (q-tag (g-tag empty-parens . pat))
                    blk)))
       #`(class-transformer
          #:single
          #,stx
          #,(parse-operator-definition #'form-id
                                       'rule
                                       stx
                                       #:allowed '(prefix)
                                       (no-srcloc #'(g-tag ignore . pat))
                                       #'blk
                                       #f
                                       #f))]
      [(named-macro (form-id . _)
                    (a-tag::alts
                     (b-tag::block
                      ((~and outer-g-tag group)
                       (q-tag::quotes ((~and g-tag group) id:identifier . pat))
                       blk))
                     ...))
       #:with (empty-parens ...) (for/list ([id (in-list (syntax->list #'(id ...)))])
                                   (datum->syntax id '(parens) id id))
       (define stx
         (respan #`(form-id
                    (a-tag
                     (b-tag
                      (outer-g-tag
                       (q-tag (g-tag empty-parens . pat)) blk))
                     ...))))
       #`(class-transformer
          #:multi
          #,stx
          #,(parse-operator-definitions #'form-id
                                        'rule
                                        #:allowed '(prefix)
                                        stx
                                        (map no-srcloc (syntax->list #'((g-tag ignore . pat) ...)))
                                        (syntax->list #'(blk ...))
                                        #f
                                        #f
                                        #f #f
                                        #'() #'() #'() '()))]
      [_
       #`(class-transformer #,g)]))
  (with-syntax ([make-prefix-operator make-prefix-operator])
    #`(make-prefix-operator #f
                            '((default . stronger))
                            'macro
                            (let ([name #,lam])
                              (let ([name (lambda (tail head #,@extra-args)
                                            (name (pack-tail
                                                   (cons head (unpack-tail tail #f #f)))))])
                                name)))))

(define-for-syntax (check-class-transformer tail-name g what)
  (syntax-parse g
    #:datum-literals (group named-macro)
    [(named-macro orig-stx (q-tag::quotes ((~and g-tag group) id:identifier . pat)) blk)
     (unless (eq? (syntax-e tail-name) (syntax-e #'id))
       (raise-syntax-error #f
                           (format "name in pattern does not match the prefixless ~a name" what)
                           #'orig-stx
                           #'id))]
    [(named-macro orig-stx (a-tag::alts
                                    (b-tag::block
                                     ((~and outer-g-tag group)
                                      (q-tag::quotes ((~and g-tag group) id:identifier . pat))
                                      blk))
                                    ...))
     (for ([id (in-list (syntax->list #'(id ...)))])
       (unless (eq? (syntax-e tail-name) (syntax-e id))
         (raise-syntax-error #f
                             (format "name in pattern does not match the prefixless ~a name" what)
                             #'orig-stx
                             id)))]
    [(named-macro orig-stx . _)
     (raise-syntax-error #f "invalid pattern form" #'orig-stx)]
    [_
     (void)]))
