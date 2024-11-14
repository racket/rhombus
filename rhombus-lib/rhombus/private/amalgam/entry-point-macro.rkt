#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "macro-result.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     "entry-point-adjustment-meta.rkt"
                     "macro-result.rkt"
                     "realm.rkt"
                     "annotation-failure.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     (submod "symbol.rkt" for-static-info)
                     "call-result-key.rkt"
                     (for-syntax racket/base))
         "space-provide.rkt"
         "entry-point.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space entry_point rhombus/entry_point
  #:fields
  (macro))

(provide (for-syntax (for-space rhombus/namespace
                                entry_point_meta)))

(begin-for-syntax
  (define-name-root entry_point_meta
    #:fields
    ([pack entry_point_meta.pack]
     [unpack entry_point_meta.unpack]
     [pack_shape entry_point_meta.pack_shape]
     [unpack_shape entry_point_meta.unpack_shape]
     Parsed
     Shape
     [Adjustment entry_point_meta.Adjustment])))

(define-identifier-syntax-definition-transformer macro
  rhombus/entry_point
  #:extra ([#:mode get-symbol-static-infos value]
           [#:adjustment get-entry-point-adjustment-static-infos value])
  #'make-entry-point-transformer)

(begin-for-syntax
  (define-transformer-syntax-class
    Parsed :entry-point #:rhombus/entry_point
    #:arity 2)
  (define-transformer-syntax-class
    Shape :entry-point-shape #:rhombus/entry_point_shape))

(define-for-syntax (extract-entry-point form proc adjustments)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [(~var ep (:entry-point adjustments)) #'ep.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "entry point function" form)]))

(define-for-syntax (extract-entry-point-shape form proc)
  (syntax->datum (check-entry-point-shape-result form proc)))

(define-for-syntax (make-entry-point-transformer proc)
  (entry-point-transformer
   (lambda (stx adjustments)
     (define form
       (syntax-parse stx
         [(head . tail) (proc (pack-tail #'tail) #'head 'function adjustments)]))
     (extract-entry-point form proc adjustments))
   (lambda (stx)
     (define form
       (syntax-parse stx
         [(head . tail) (proc (pack-tail #'tail) #'head 'shape #f)]))
     (extract-entry-point-shape form proc))))

(define-for-syntax (check-syntax who s)
  (unless (syntax? s)
    (raise-annotation-failure who s "Syntax")))

(begin-for-syntax
  (define/arity (entry_point_meta.pack stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    #`(parsed #:rhombus/entry_point
              (rhombus-expression #,(unpack-group stx who #f))))

  (define/arity (entry_point_meta.unpack stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse stx
      #:datum-literals (parsed)
      [(parsed #:rhombus/entry_point e)
       #'(parsed #:rhombus/expr e)]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a parsed entry point function"
                                 "syntax object" stx)]))

  (define/arity (entry_point_meta.pack_shape a)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    #`(parsed #:rhombus/entry_point_shape #,(check-entry-point-shape-result a entry_point_meta.unpack_shape)))

  (define/arity (entry_point_meta.unpack_shape stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse stx
      #:datum-literals (parsed)
      [(parsed #:rhombus/entry_point_shape a) (syntax->datum #'a)]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a parsed entry point shape"
                                 "syntax object" stx)])))
