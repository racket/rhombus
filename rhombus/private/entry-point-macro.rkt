#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "macro-result.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     "entry-point-adjustment.rkt"
                     "macro-result.rkt"
                     "realm.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
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
     Parsed
     Arity
     [Adjustment entry_point_meta.Adjustment])))

(define-identifier-syntax-definition-transformer macro
  rhombus/entry_point
  #:extra ([#:mode (quote-syntax ()) value]
           [#:adjustment (get-entry-point-adjustment-static-infos) value])
  #'make-entry-point-transformer)

(begin-for-syntax
  (define-transformer-parameterized-syntax-class
    Parsed :entry-point #:rhombus/entry_point)
  (define-transformer-syntax-class
    Arity :entry-point-arity #:rhombus/entry_point))

(define-for-syntax (extract-entry-point form proc adjustments)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [(~var ep (:entry-point adjustments)) #'ep.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "entry point function" form)]))

(define-for-syntax (extract-entry-point-arity form proc)
  (syntax->datum (check-entry-point-arity-result form proc)))

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
         [(head . tail) (proc (pack-tail #'tail) #'head 'arity #f)]))
     (extract-entry-point-arity form proc))))

(define-for-syntax (check-syntax who s)
  (unless (syntax? s)
    (raise-argument-error* who rhombus-realm "Syntax" s)))

(begin-for-syntax
  (define/arity (entry_point_meta.pack stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    #`(parsed #:rhombus/entry_point
              (rhombus-expression #,(unpack-group stx who #f))))
  )
